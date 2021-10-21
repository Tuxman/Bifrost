package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.codecs.binary.valuetypes.codecs._
import org.scalacheck.Gen

class IntCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[Int](
    "int",
    _ => intCodec,
    int => writer => writer.putInt(int),
    _ => reader => reader.getInt(),
    Gen.chooseNum[Int](Int.MinValue, Int.MaxValue)
  )
}
