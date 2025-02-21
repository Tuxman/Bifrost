package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.function.FunctionAdapter1
import org.scalamock.handlers.{CallHandler1, CallHandler2, CallHandler3}

import scala.annotation.tailrec

object TestHelper extends TransactionGenerator {

  implicit class CallHandler1Ops[T1, R](ch: CallHandler1[T1, R]) {
    def rep(count: Int): CallHandler1[T1, R] = ch.repeated(count to count)
  }

  implicit class CallHandler2Ops[T1, T2, R](ch: CallHandler2[T1, T2, R]) {
    def rep(count: Int): CallHandler2[T1, T2, R] = ch.repeated(count to count)
  }

  implicit class CallHandler3Ops[T1, T2, T3, R](ch: CallHandler3[T1, T2, T3, R]) {
    def rep(count: Int): CallHandler3[T1, T2, T3, R] = ch.repeated(count to count)
  }

  @tailrec
  private def addHeaderToChain(
    headers: NonEmptyChain[BlockHeader],
    gen:     Gen[BlockHeader],
    count:   Long
  ): NonEmptyChain[BlockHeader] =
    count match {
      case 0 => headers
      case _ =>
        val parentId = headers.last.id
        addHeaderToChain(headers.append(gen.sample.get.copy(parentHeaderId = parentId)), gen, count - 1)
    }

  val arbitraryHost: Arbitrary[HostId] = Arbitrary(Gen.identifier)

  val arbitraryHostBlockId: Arbitrary[(HostId, BlockId)] = Arbitrary(
    for {
      host    <- arbitraryHost.arbitrary
      blockId <- arbitraryBlockId.arbitrary
    } yield (host, blockId)
  )

  def arbitraryLinkedBlockHeaderChain(sizeGen: Gen[Long]): Arbitrary[NonEmptyChain[BlockHeader]] =
    Arbitrary(
      for {
        size <- sizeGen
        headerGen = ModelGenerators.arbitraryHeader.arbitrary.map(_.embedId)
        root <- headerGen
      } yield addHeaderToChain(NonEmptyChain.one(root), headerGen, size)
    )

  val maxTxsCount = 5

  implicit val arbitraryTxsAndBlock: Arbitrary[(Seq[IoTransaction], BlockBody)] =
    Arbitrary(
      for {
        txs <- Gen.listOfN(maxTxsCount, arbitraryIoTransaction.arbitrary.map(_.embedId))
        // TODO: Reward
      } yield (txs, BlockBody(txs.map(tx => tx.id)))
    )

  def headerToSlotData(header: BlockHeader): SlotData = {
    val sampleSlotData = ModelGenerators.arbitrarySlotData.arbitrary.first
    val slotId = sampleSlotData.slotId.copy(blockId = header.id)
    val parentSlotId = sampleSlotData.parentSlotId.copy(blockId = header.parentHeaderId)
    sampleSlotData.copy(slotId = slotId, parentSlotId = parentSlotId)
  }

  def arbitraryLinkedSlotDataHeaderBlockNoTx(
    sizeGen: Gen[Long]
  ): Arbitrary[NonEmptyChain[(BlockId, SlotData, BlockHeader, BlockBody)]] =
    Arbitrary(
      for {
        size    <- sizeGen
        headers <- arbitraryLinkedBlockHeaderChain(Gen.oneOf(List[Long](size))).arbitrary
      } yield NonEmptyChain
        .fromSeq(headers.foldLeft(List.empty[(BlockId, SlotData, BlockHeader, BlockBody)]) { case (blocks, header) =>
          val body = co.topl.models.generators.node.ModelGenerators.arbitraryNodeBody.arbitrary.first
          val headerWithTxRoot = header.copy(txRoot = body.merkleTreeRootHash.data)
          if (blocks.isEmpty) {
            List((headerWithTxRoot.id, headerToSlotData(headerWithTxRoot), headerWithTxRoot, body))
          } else {
            val headerWithParent = headerWithTxRoot.copy(parentHeaderId = blocks.last._2.slotId.blockId).embedId
            blocks.appended((headerWithParent.id, headerToSlotData(headerWithParent), headerWithParent, body))
          }
        })
        .get
    )

  def compareDownloadedHeaderWithoutDownloadTimeMatcher(
    rawExpectedMessage: RequestsProxy.Message
  ): FunctionAdapter1[RequestsProxy.Message, Boolean] = {
    val matchingFunction: RequestsProxy.Message => Boolean =
      (rawActualMessage: RequestsProxy.Message) =>
        (rawExpectedMessage, rawActualMessage) match {
          case (
                expectedMessage: RequestsProxy.Message.DownloadHeadersResponse,
                actualMessage: RequestsProxy.Message.DownloadHeadersResponse
              ) =>
            val newResp =
              actualMessage.response.map { case (header, res) =>
                (header, res.map(b => b.copy(downloadTimeMs = 0)))
              }
            expectedMessage == actualMessage.copy(response = newResp)
          case (_, _) => throw new IllegalStateException("Unexpected case")
        }
    new FunctionAdapter1[RequestsProxy.Message, Boolean](matchingFunction)
  }

  def compareDownloadedBodiesWithoutDownloadTimeMatcher(
    rawExpectedMessage: RequestsProxy.Message
  ): FunctionAdapter1[RequestsProxy.Message, Boolean] = {
    val matchingFunction: RequestsProxy.Message => Boolean =
      (rawActualMessage: RequestsProxy.Message) =>
        (rawExpectedMessage, rawActualMessage) match {
          case (
                expectedMessage: RequestsProxy.Message.DownloadBodiesResponse,
                actualMessage: RequestsProxy.Message.DownloadBodiesResponse
              ) =>
            val newResp =
              actualMessage.response.map { case (header, res) =>
                (header, res.map(b => b.copy(downloadTimeMs = 0, downloadTimeTxMs = Seq.empty)))
              }
            expectedMessage == actualMessage.copy(response = newResp)
          case (_, _) => throw new IllegalStateException("Unexpected case")
        }
    new FunctionAdapter1[RequestsProxy.Message, Boolean](matchingFunction)
  }
}
