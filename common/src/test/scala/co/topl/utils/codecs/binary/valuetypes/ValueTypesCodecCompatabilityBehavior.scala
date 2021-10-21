package co.topl.utils.codecs.binary.valuetypes

import akka.util.ByteString
import cats.{Eq, Show}
import co.topl.utils.EqMatcher
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.Codec
import scodec.bits.BitVector
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, Reader, VLQByteStringReader, VLQByteStringWriter, Writer}

trait ValueTypesCodecCompatabilityBehavior extends AnyFlatSpec with EqMatcher with ScalaCheckDrivenPropertyChecks {

  def valueTypesCodecCompatabilityBehavior[T: Show: Eq](
    name:           String,
    codecGen:       T => Codec[T],
    vlqSerialize:   T => VLQByteStringWriter => VLQByteStringWriter,
    vlqDeserialize: T => VLQByteStringReader => T,
    generator:      Gen[T]
  ): Unit = {
    behavior of (name + " serialization compatability")

    it should "generate output of codec encode that can be decoded by Bifrost serializer" in {
      forAll(generator) { value =>
        val encodedValue = codecGen(value).encode(value).getOrThrow()

        val encodedByteString = ByteString.fromArray(encodedValue.toByteArray)

        val decodedValue =
          vlqDeserialize(value)(new VLQByteStringReader(encodedByteString))

        decodedValue should eqShow(value)
      }
    }

    it should "generate output of Bifrost serializer encode that can be decoded by codec" in {
      forAll(generator) { value =>
        val encodedValue = vlqSerialize(value)(new VLQByteStringWriter()).toBytes

        val encodedBitVector = BitVector(encodedValue)

        val decodedValue = codecGen(value).decode(encodedBitVector).map(_.value).getOrThrow()

        decodedValue should eqShow(value)
      }
    }

    it should "be able to decode an encoded value" in {
      forAll(generator) { value =>
        val codec = codecGen(value)

        val encodedValue = codec.encode(value).getOrThrow()

        val decodedValue = codec.decode(encodedValue).map(_.value).getOrThrow()

        decodedValue should eqShow(value)
      }
    }

    it should "have no bits left over when decoding an encoded value" in {
      forAll(generator) { value =>
        val codec = codecGen(value)

        val encodedValue = codec.encode(value).getOrThrow()

        val remaining = codec.decode(encodedValue).map(_.remainder).getOrThrow()

        remaining should have length 0
      }
    }
  }
}
