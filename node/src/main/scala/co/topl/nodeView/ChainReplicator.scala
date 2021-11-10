package co.topl.nodeView

import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import cats.data.OptionT
import cats.implicits._
import co.topl.modifier.ModifierId
import co.topl.modifier.ModifierId.fromBase58
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.ChainReplicatorSettings
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.implicits._
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel, UnconfirmedTransactionDataModel}
import com.mongodb.client.result.{DeleteResult, InsertManyResult}
import org.bson.BsonValue
import org.mongodb.scala.bson.Document
import org.slf4j.Logger

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success}

/**
 * Chain replicator listens to info about new blocks from NodeViewHolder and updates the AppView with new blocks
 * It could also find the previously missing blocks and send them to the AppView
 */
object ChainReplicator {

  val actorName = "ChainReplicator"

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    private[nodeView] case class GotNewBlock(block: Block) extends ReceivableMessage

    private[nodeView] case class CheckMissingBlocks(startHeight: Long, maxHeight: Long) extends ReceivableMessage

    private[nodeView] case class CheckMissingBlocksDone(startHeight: Long, endHeight: Long, maxHeight: Long)
        extends ReceivableMessage

    private[nodeView] case object CheckDatabaseComplete extends ReceivableMessage

    private[nodeView] case class Terminate(reason: Throwable) extends ReceivableMessage
  }

  def apply(
    nodeViewHolderRef:    ActorRef[NodeViewHolder.ReceivableMessage],
    checkDBConnection:    () => Future[Seq[String]],
    insertDB:             (Seq[Document], String) => Future[InsertManyResult],
    removeDB:             (String, Seq[String], String) => Future[DeleteResult],
    getUnconfirmedTxDB:   String => Future[Seq[String]],
    getMissingBlockIdsDB: (Seq[String], String) => Future[Seq[String]],
    settings:             ChainReplicatorSettings
  ): Behavior[ReceivableMessage] =
    Behaviors.withStash(settings.actorStashSize) { buffer =>
      Behaviors.setup { implicit context =>
        implicit val ec: ExecutionContext = context.executionContext

        context.system.eventStream.tell(
          EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
            context.messageAdapter(block => ReceivableMessages.GotNewBlock(block.modifier))
          )
        )

        context.log.info(s"${Console.GREEN}Chain replicator initializing${Console.RESET}")

        context.pipeToSelf(checkDBConnection()) {
          case Success(result) =>
            context.log.info(s"${Console.GREEN}Found collections in database: $result${Console.RESET}")
            ReceivableMessages.CheckDatabaseComplete
          case Failure(e) => ReceivableMessages.Terminate(e)
        }

        new ChainReplicator(
          nodeViewHolderRef,
          buffer,
          insertDB,
          removeDB,
          getUnconfirmedTxDB,
          getMissingBlockIdsDB,
          settings
        ).uninitialized

      }
    }
}

private class ChainReplicator(
  nodeViewHolderRef:    ActorRef[NodeViewHolder.ReceivableMessage],
  buffer:               StashBuffer[ChainReplicator.ReceivableMessage],
  insertDB:             (Seq[Document], String) => Future[InsertManyResult],
  removeDB:             (String, Seq[String], String) => Future[DeleteResult],
  getUnconfirmedTxDB:   (String) => Future[Seq[String]],
  getMissingBlockIdsDB: (Seq[String], String) => Future[Seq[String]],
  settings:             ChainReplicatorSettings
)(implicit
  context: ActorContext[ChainReplicator.ReceivableMessage]
) {
  implicit private val log: Logger = context.log

  import ChainReplicator._

  /** The actor waits for the database check to complete, stops if chain repliactor is turned off in settings */
  def uninitialized: Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.CheckDatabaseComplete =>
        log.info(s"${Console.GREEN}Chain replicator is ready${Console.RESET}")
        if (settings.checkMissingBlock) {
          val bestBlockHeight = withNodeView(getBestBlockHeight)
          context.pipeToSelf(bestBlockHeight) {
            case Success(height) =>
              if (settings.checkMissingStartHeight <= height)
                ReceivableMessages.CheckMissingBlocks(settings.checkMissingStartHeight, height)
              else
                ReceivableMessages.CheckMissingBlocks(height, height)
            case Failure(e) => ReceivableMessages.Terminate(e)
          }
          log.info(s"${Console.GREEN}Chain replicator transitioning to syncing${Console.RESET}")
          buffer.unstashAll(syncing)
        } else {
          log.info(s"${Console.GREEN}Chain replicator transitioning to listening for new blocks${Console.RESET}")
          buffer.unstashAll(listening)
        }

      case ReceivableMessages.Terminate(reason) =>
        throw reason

      case other =>
        buffer.stash(other)
        Behaviors.same
    }

  /** Find and send missing blocks to AppView, and transition to listening for new blocks once all checks are done */
  private def syncing: Behavior[ReceivableMessage] =
    Behaviors.receivePartial[ReceivableMessage] {
      case (context, ReceivableMessages.CheckMissingBlocks(startHeight, maxHeight)) =>
        implicit val ec: ExecutionContext = context.executionContext

        val endHeight = (startHeight + settings.blockCheckSize).min(maxHeight)
        val exportResult = for {
          blockIdsInRange <- OptionT[Future, Seq[String]](getIdsAtHeights(startHeight, endHeight).map(_.some))
          missingBlockIds <- OptionT[Future, Seq[String]](
            getMissingBlockIdsDB(blockIdsInRange, settings.blockCollection).map(_.some)
          )
          blocksToAdd       <- OptionT[Future, Seq[Block]](getBlocksById(missingBlockIds))
          blockExportResult <- OptionT[Future, InsertManyResult](exportBlocks(blocksToAdd).map(_.some))
          txExportResult    <- OptionT[Future, InsertManyResult](exportTxsFromBlocks(blocksToAdd).map(_.some))
        } yield (blockExportResult, txExportResult)

        context.pipeToSelf(exportResult.value) {
          case Success(Some((blockExportResult, txExportResult))) =>
            log.info(
              s"${Console.GREEN}Successfully inserted ${blockExportResult.getInsertedIds.size()} blocks " +
              s"and ${txExportResult.getInsertedIds.size()} transactions into AppView${Console.RESET}"
            )
            ReceivableMessages.CheckMissingBlocksDone(startHeight, endHeight, maxHeight)
          case Failure(err) =>
            ReceivableMessages.Terminate(err)
          case _ =>
            log.info(
              s"${Console.GREEN}No missing blocks found between $startHeight and $endHeight${Console.RESET}"
            )
            ReceivableMessages.CheckMissingBlocksDone(startHeight, endHeight, maxHeight)
        }

        Behaviors.same

      case (context, ReceivableMessages.CheckMissingBlocksDone(startHeight, endHeight, maxHeight)) =>
        log.info(s"${Console.GREEN}Finished checking from height $startHeight to $endHeight${Console.RESET}")
        if (endHeight >= maxHeight) {
          log.info(s"Done with all checks at height $endHeight")
          buffer.unstashAll(listening)
        } else {
          context.self ! ReceivableMessages.CheckMissingBlocks(endHeight + 1, maxHeight)
          Behaviors.same
        }

      case (_, ReceivableMessages.Terminate(reason)) =>
        throw reason

      case (_, other) =>
        buffer.stash(other)
        Behaviors.same
    }

  /**
   * The actor exports new blocks from NodeViewHolder
   * If checkMissingBlock is turned on, start finding and exporting blocks at missing heights found in a height range
   * in the database after set amount of time
   */
  private def listening: Behavior[ReceivableMessage] =
    Behaviors.receivePartial[ReceivableMessage] {
      case (context, ReceivableMessages.GotNewBlock(block)) =>
        implicit val ec: ExecutionContext = context.executionContext
        val exportResult = for {
          blockExportResult        <- exportBlocks(Seq(block))
          txExportResult           <- exportTxsFromBlocks(Seq(block))
          unconfirmedInDB          <- getUnconfirmedTxDB(settings.unconfirmedTxCollection)
          (txToInsert, txToRemove) <- getTxToInsertRemove(unconfirmedInDB)
          unconfirmedTxExportRes   <- exportUnconfirmedTxs(txToInsert)
          unconfirmedTxRemoveRes   <- removeDB("txId", txToRemove, settings.unconfirmedTxCollection)
        } yield (blockExportResult, txExportResult, unconfirmedTxExportRes, unconfirmedTxRemoveRes)
        exportResult.onComplete {
          case Success(res) =>
            log.info(
              s"${Console.GREEN}Added a block and ${res._2.getInsertedIds.size()} " +
              s"transactions to appView at height: ${block.height}${Console.RESET}"
            )
            log.info(
              s"${Console.GREEN}Added ${res._3.getInsertedIds.size()} transactions and removed " +
              s"${res._4.getDeletedCount} transactions from the unconfirmed transactions in appView${Console.RESET}"
            )
          case Failure(err) =>
            log.error(s"${Console.RED}$err${Console.RESET}")
        }
        Behaviors.same

      case (_, ReceivableMessages.Terminate(reason)) =>
        throw reason
    }

  /**
   * Export a sequence of blocks to the AppView
   * @param blocks sequence of blocks to be inserted in AppView
   * @return Insertion result from the database
   */
  private def exportBlocks(blocks: Seq[Block]): Future[InsertManyResult] = {
    implicit val ec: ExecutionContext = context.executionContext
    insertDB(blocks.map(BlockDataModel(_).asDocument), settings.blockCollection)
  }

  /**
   * Export transactions from a sequence of blocks to the AppView
   * @param blocks sequence of blocks that need transactions inserted in AppView
   * @return Insertion result from the database
   */
  private def exportTxsFromBlocks(blocks: Seq[Block]): Future[InsertManyResult] = {
    implicit val ec: ExecutionContext = context.executionContext
    val txDocs = blocks.flatMap { b =>
      b.transactions.map { tx =>
        ConfirmedTransactionDataModel(b.id.toString, b.height, tx).asDocument
      }
    }
    insertDB(txDocs, settings.confirmedTxCollection)
  }

  /**
   * Export unconfirmed transactions from mempool to the AppView
   * @param txs sequence of transactions that need to be send to the AppView
   * @return Insertion result from the database
   */
  private def exportUnconfirmedTxs(txs: Seq[Transaction.TX]): Future[InsertManyResult] = {
    implicit val ec: ExecutionContext = context.executionContext
    if (txs.nonEmpty)
      insertDB(txs.map(tx => UnconfirmedTransactionDataModel(tx).asDocument), settings.unconfirmedTxCollection)
    else
      Future.successful(InsertManyResult.acknowledged(Map[Integer, BsonValue]().asJava))
  }

  /**
   * Given block id strings, fetch the blocks of corresponding ids from nodeView
   * @param blockIds id strings of blocks needed
   * @return blocks of given ids
   */
  private def getBlocksById(blockIds: Seq[String]): Future[Option[Seq[Block]]] = {
    def blocksById(blockIds: Seq[String])(nodeView: ReadableNodeView): Option[Seq[Block]] =
      if (blockIds.nonEmpty)
        blockIds.flatMap { idString =>
          val id = ModifierId.fromBase58(Base58Data.unsafe(idString))
          nodeView.history.modifierById(id)
        }.some
      else
        None

    withNodeView(blocksById(blockIds))
  }

  /**
   * Give an start height and end height, find ids of all blocks in this range
   * @param startHeight start height
   * @param endHeight end height
   * @return ids of blocks between start height and end height
   */
  private def getIdsAtHeights(startHeight: Long, endHeight: Long): Future[Seq[String]] = {
    def idsAtHeights(startHeight: Long, endHeight: Long)(nodeView: ReadableNodeView): Seq[String] =
      (startHeight to endHeight).flatMap(nodeView.history.idAtHeightOf).map(_.toString)
    withNodeView(idsAtHeights(startHeight, endHeight))
  }

  /**
   * Gets the height of the best block
   * @param nodeView NodeView
   * @return height of the current best block
   */
  private def getBestBlockHeight(nodeView: ReadableNodeView): Long =
    nodeView.history.height

  /**
   * Access the NodeView and gets the blocks at given heights
   * @param blockHeights the heights of the blocks needed
   * @param nodeView NodeView
   * @return optional sequence of blocks at given heights
   */
  private def blocksAtHeight(blockHeights: Seq[Long])(nodeView: ReadableNodeView): Option[Seq[Block]] = {
    val blocks = blockHeights.flatMap(nodeView.history.modifierByHeight)
    if (blocks.nonEmpty) blocks.some
    else None
  }

  /**
   * Get transactions to insert and remove in order to have the same unconfirmed transactions in appView and nodeView
   * @param unconfirmedTxDB current unconfirmed transactions in the appView
   * @return transactions to export to appView, and ids of unconfirmed transactions to remove in the appView
   */
  private def getTxToInsertRemove(unconfirmedTxDB: Seq[String]): Future[(Seq[Transaction.TX], Seq[String])] =
    withNodeView(compareMempool(unconfirmedTxDB))

  /**
   * Compare unconfirmed transactions between nodeView and appView
   * @param unconfirmedTxDB current unconfirmed transactions in the appView
   * @param nodeView NodeView
   * @return transactions to export to appView, and ids of unconfirmed transactions to remove in the appView
   */
  private def compareMempool(unconfirmedTxDB: Seq[String])(
    nodeView:                                 ReadableNodeView
  ): (Seq[Transaction.TX], Seq[String]) = {
    val mempool = nodeView.memPool
    val toRemove = unconfirmedTxDB.filter { txString =>
      mempool.modifierById(fromBase58(Base58Data.unsafe(txString))).isEmpty
    }
    val toInsert = mempool
      .take(settings.mempoolCheckSize)(-_.dateAdded)
      .map(_.tx)
      .filterNot { tx =>
        unconfirmedTxDB.contains(tx.id.toString)
      }
      .toSeq
    (toInsert, toRemove)
  }

  /**
   * Function for accessing the nodeView
   * @param f function to be applied on nodeView
   * @tparam T result type
   * @return result from nodeView
   */
  private def withNodeView[T](f: ReadableNodeView => T): Future[T] = {
    import akka.actor.typed.scaladsl.AskPattern._

    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(10.seconds)
    implicit val typedSystem: ActorSystem[_] = context.system
    nodeViewHolderRef.askWithStatus[T](NodeViewHolder.ReceivableMessages.Read(f, _))
  }
}
