package co.topl.ledger

import cats.implicits._
import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.codecs.bytes.ByteCodecInstances._
import co.topl.ledger.Persistence.implicits._
import co.topl.models._
import co.topl.typeclasses.ChainableValidation.Instances._
import co.topl.typeclasses.ChainableValidation.ops._
import co.topl.typeclasses.ContainsHeight.Instances._
import co.topl.typeclasses.ContainsHeight.ops._
import co.topl.typeclasses.ContainsParent.Instances._
import co.topl.typeclasses.ContainsParent.ops._
import co.topl.typeclasses.ContainsTransactions.Instances._
import co.topl.typeclasses.ContainsTransactions.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

import scala.collection.immutable.ArraySeq

case class Ledger(
  private val boxPersistence:         Persistence,
  private val addressPersistence:     Persistence,
  private val blockPersistence:       Persistence,
  private val transactionPersistence: Persistence,
  private val metaPersistence:        Persistence,
  currentHead:                        Block
) extends Seq[Block] {

  def withBlock(block: Block): Either[Ledger.Failure, Ledger] =
    Either
      .cond(block.isChildOf(currentHead), block, Ledger.InvalidBlockParentType(block, currentHead))
      .map(block =>
        applyStateModifications(block)
          .applyTransactionModifications(block)
          .applyBlockModifications(block)
          .applyMetaModifications(block)
          .copy(currentHead = block)
      )

  private def applyStateModifications(block: Block): Ledger = ???
  private def applyBlockModifications(block: Block): Ledger = ???

  private def applyTransactionModifications(block: Block): Ledger =
    copy(
      transactionPersistence = transactionPersistence.write(
        block.id.bytes,
        block.transactions.map(tx => tx.id.bytes -> Some(tx.bytes))
      )
    )

  private def applyMetaModifications(block: Block): Ledger = {
    val idBytes = block.id.bytes
    copy(
      metaPersistence = metaPersistence.write(idBytes, List(Ledger.BestBlockIdKey -> Some(idBytes)))
    )
  }

  def rollback(): Either[Ledger.Failure, Ledger] =
    currentHead.parentId
      .toRight(Ledger.CurrentlyAtGenesis)
      .flatMap(parentId => getBlock(parentId).toRight(Ledger.NonExistentParent(parentId)))
      .map { parent =>
        val parentIdBytes = parent.id.bytes
        Ledger(
          boxPersistence.rollbackTo(parentIdBytes),
          addressPersistence.rollbackTo(parentIdBytes),
          blockPersistence.rollbackTo(parentIdBytes),
          transactionPersistence.rollbackTo(parentIdBytes),
          metaPersistence.rollbackTo(parentIdBytes),
          parent
        )
      }

  def getBlock(blockId: TypedIdentifier): Option[Block] =
    blockPersistence.read(List(blockId.bytes)).headOption.flatMap(_._2).map(_.decoded[Block])

  def getBlockAtHeight(height: Long): Option[Block] =
    blockPersistence
      .read(List(new Bytes("height".bytes.unsafeArray ++ BigInt(height).toByteArray)))
      .headOption
      .flatMap(_._2)
      .map(_.decoded[Block])

  def getTransaction(transactionId: TypedIdentifier): Option[Transaction] =
    blockPersistence.read(List(transactionId.bytes)).headOption.flatMap(_._2).map(_.decoded[Transaction])

  def getBox(boxId: TypedIdentifier): Option[Box] =
    blockPersistence.read(List(boxId.bytes)).headOption.flatMap(_._2).map(_.decoded[Box])

  def boxesForAddress(address: Address): Either[Ledger.Failure, List[Box]] =
    boxesForAddress(address.bytes)

  def boxesForAddress(address: TaktikosAddress): Either[Ledger.Failure, List[Box]] =
    boxesForAddress(address.bytes)

  private def boxesForAddress(bytes: Bytes): Either[Ledger.Failure, List[Box]] =
    addressPersistence
      .read(List(bytes))
      .headOption
      .flatMap(_._2)
      .map(_.decoded[List[TypedIdentifier]])
      .getOrElse(Nil)
      .traverse(id => getBox(id).toRight(Ledger.BoxNotFound(id)))

  override def length: Int = (currentHead.height - 1).toInt

  override def iterator: Iterator[Block] =
    iteratorFromHeight(1)

  def iteratorFromHeight(height: Long): Iterator[Block] =
    Iterator
      .iterate(getBlockAtHeight(height))(_.map(_.height + 1).flatMap(getBlockAtHeight))
      .takeWhile(_.nonEmpty)
      .collect { case Some(b) => b }

  def iteratorFromBlock(blockId: TypedIdentifier): Iterator[Block] =
    Iterator
      .iterate(getBlock(blockId))(_.map(_.height + 1).flatMap(getBlockAtHeight))
      .takeWhile(_.nonEmpty)
      .collect { case Some(b) => b }

  def reverseIteratorFromBlock(blockId: TypedIdentifier): Iterator[Block] =
    Iterator
      .iterate(getBlock(blockId))(_.map(_.parentId).flatMap(getBlock))
      .takeWhile(_.nonEmpty)
      .collect { case Some(b) => b }
}

object Ledger {
  sealed abstract class Failure
  case class ParentHeadMismatch(currentHeadId: TypedIdentifier, newBlockId: TypedIdentifier) extends Failure
  case object CurrentlyAtGenesis extends Failure
  case class NonExistentParent(parentBlockId: TypedIdentifier) extends Failure
  case class BoxNotFound(boxId: TypedIdentifier) extends Failure
  case class InvalidBlockParentType(block: Block, parentBlock: Block) extends Failure

  final val BestBlockIdKey: Bytes = new ArraySeq.ofByte(Array.fill(32)(0: Byte))
}
