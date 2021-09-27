package co.topl.modifier.transaction.builder

import cats.implicits._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.BoxReader
import co.topl.modifier.box.{PolyBox, ProgramId}
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class TransferBuilderSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  val random = new Random()

  "buildUnsignedPolyTransfer" should "use all boxes when using 'All' strategy" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val boxReader = mock[BoxReader[ProgramId, Address]]

        val polyBoxes = random.shuffle(firstBox :: otherBoxes)

        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyBoxes))

        val request = TransferRequests.PolyTransferRequest(
          List(sender),
          List(recipient -> polyBoxes.map(_.value.quantity).sum),
          sender,
          0,
          None
        )

        val buildResult =
          TransferBuilder
            .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

        // check that the same nonces are in the result as in the inputs
        buildResult.value.from.map(_._2).sorted shouldBe polyBoxes.map(_.nonce).sorted
    }
  }

  it should "use the specified boxes when using 'Specific' strategy" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (specificBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val boxReader = mock[BoxReader[ProgramId, Address]]

        val polyBoxes = random.shuffle(specificBox :: otherBoxes)

        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyBoxes))

        val request = TransferRequests.PolyTransferRequest(
          List(sender),
          List(recipient -> specificBox.value.quantity),
          sender,
          0,
          None
        )

        val buildResult =
          TransferBuilder
            .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
              boxReader,
              request,
              BoxSelectionAlgorithms.Specific(List(specificBox.id))
            )

        // check that the nonces are the same
        buildResult.value.from.map(_._2).sorted shouldBe List(specificBox).map(_.nonce).sorted
    }
  }

  it should "use the smallest boxes first when using 'SmallestFirst' strategy" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val boxReader = mock[BoxReader[ProgramId, Address]]

        val polyBoxes = random.shuffle(firstBox :: otherBoxes)

        val smallestBox = polyBoxes.sorted(Ordering.by((x: PolyBox) => x.value.quantity)).head

        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyBoxes))

        val request = TransferRequests.PolyTransferRequest(
          List(sender),
          List(recipient -> smallestBox.value.quantity),
          sender,
          0,
          None
        )

        val buildResult =
          TransferBuilder
            .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
              boxReader,
              request,
              BoxSelectionAlgorithms.SmallestFirst
            )

        // check that the only box used is the smallest one
        buildResult.value.from.map(_._2).sorted shouldBe List(smallestBox).map(_.nonce).sorted
    }
  }

  it should "use the largest boxes first when using 'LargestFirst' strategy" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val boxReader = mock[BoxReader[ProgramId, Address]]

        val polyBoxes = random.shuffle(firstBox :: otherBoxes)

        val largestBox = polyBoxes.sorted(Ordering.by((x: PolyBox) => -x.value.quantity)).head

        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyBoxes))

        val request = TransferRequests.PolyTransferRequest(
          List(sender),
          List(recipient -> largestBox.value.quantity),
          sender,
          0,
          None
        )

        val buildResult =
          TransferBuilder
            .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
              boxReader,
              request,
              BoxSelectionAlgorithms.LargestFirst
            )

        // check that the only box used is the smallest one
        buildResult.value.from.map(_._2).sorted shouldBe List(largestBox).map(_.nonce).sorted
    }
  }
}
