package co.topl.crypto.hash

import scala.util.Try

/* Forked from https://github.com/input-output-hk/scrypto */

trait CryptographicHash32 extends CryptographicHash[Digest32] {

  override val DigestSize: Int = 32

  override def byteArrayToDigest(bytes: Array[Byte]): Try[Digest32] = Try {
    require(bytes.lengthCompare(DigestSize) == 0, "Incorrect digest size")
    Digest32 @@ bytes
  }
}
