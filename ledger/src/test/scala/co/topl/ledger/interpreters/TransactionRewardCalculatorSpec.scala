package co.topl.ledger.interpreters

import cats.effect.IO
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.box.Attestation
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction._
import co.topl.models.ModelGenerators.GenHelper
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import quivr.models.Int128

class TransactionRewardCalculatorSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  test("TransactionRewardCalculator returns empty list for empty Tx") {
    val tx = IoTransaction.defaultInstance
    val testResource =
      for {
        underTest <- TransactionRewardCalculator.make[F]
        _         <- underTest.rewardOf(tx).assertEquals(BigInt(0)).toResource
      } yield ()

    testResource.use_
  }

  test("TransactionRewardCalculator returns a single value for unclaimed LVLs") {
    val inputs =
      List(BigInt(500), BigInt(800))
        .map(quantity => Value(Value.Value.Lvl(Value.LVL(Int128(ByteString.copyFrom(quantity.toByteArray))))))
        .map(value =>
          SpentTransactionOutput(arbitraryTransactionOutputAddress.arbitrary.first, Attestation.defaultInstance, value)
        )

    val outputs =
      List(BigInt(100))
        .map(quantity => Value(Value.Value.Lvl(Value.LVL(Int128(ByteString.copyFrom(quantity.toByteArray))))))
        .map(value => UnspentTransactionOutput(arbitraryLockAddress.arbitrary.first, value))
    val tx = IoTransaction.defaultInstance.withInputs(inputs).withOutputs(outputs)
    val testResource =
      for {
        underTest <- TransactionRewardCalculator.make[F]
        rewards   <- underTest.rewardOf(tx).toResource
        _         <- IO(rewards).assertEquals(BigInt(1200)).toResource
      } yield ()

    testResource.use_
  }

  test("TransactionRewardCalculator returns an empty list if LVL outputs exceed LVL inputs") {
    val inputs =
      List(BigInt(500), BigInt(800))
        .map(quantity => Value(Value.Value.Lvl(Value.LVL(Int128(ByteString.copyFrom(quantity.toByteArray))))))
        .map(value =>
          SpentTransactionOutput(arbitraryTransactionOutputAddress.arbitrary.first, Attestation.defaultInstance, value)
        )

    val outputs =
      List(BigInt(1500))
        .map(quantity => Value(Value.Value.Lvl(Value.LVL(Int128(ByteString.copyFrom(quantity.toByteArray))))))
        .map(value => UnspentTransactionOutput(arbitraryLockAddress.arbitrary.first, value))
    val tx = IoTransaction.defaultInstance.withInputs(inputs).withOutputs(outputs)
    val testResource =
      for {
        underTest <- TransactionRewardCalculator.make[F]
        _         <- underTest.rewardOf(tx).assertEquals(BigInt(0)).toResource
      } yield ()

    testResource.use_
  }
}
