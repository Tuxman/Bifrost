package co.topl.attestation

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.settings.NetworkType
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scorex.util.encode.Base58

class AddressSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators {

  property("Applying address string with incorrect networkPrefix will result in error") {
    forAll(propositionGen) { pubkey: PublicKeyPropositionCurve25519 =>
      val twoNetworkType = scala.util.Random.shuffle(NetworkType.all).take(2)
      val fstNetworkType: NetworkType = twoNetworkType.head
      val secNetworkType: NetworkType = twoNetworkType.last

      implicit var networkPrefix: NetworkPrefix = fstNetworkType.netPrefix
      val address: Address = pubkey.address
      val addrStr = address.toString

      networkPrefix = secNetworkType.netPrefix
      val thrown = intercept[Exception] {
        Address(networkPrefix)(addrStr)
      }
      thrown.getMessage shouldEqual "Invalid address: Network type does not match"
    }
  }

  property("Applying address with a pair of incorrect content and checksum will result in error") {
    forAll(propositionGen) { pubkey: PublicKeyPropositionCurve25519 =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.MainNet.netPrefix
      val address: Address = pubkey.address
      val addrStr: String = address.toString
      val addrByte: Array[Byte] = Base58.decode(addrStr).get
      val corruptByte: Byte = (addrByte(2).toInt + 1).toByte
      val modedAddrByte: Array[Byte] = addrByte.slice(0,2) ++ Array(corruptByte) ++ addrByte.slice(3,addrByte.length)
      val modedAddrStr: String = Base58.encode(modedAddrByte)

      assert(!(addrByte sameElements modedAddrByte))

      val thrown = intercept[Exception] {
        Address(networkPrefix)(modedAddrStr)
      }
      thrown.getMessage shouldEqual s"requirement failed: Invalid address: Checksum fails for $modedAddrStr"
    }
  }
}
