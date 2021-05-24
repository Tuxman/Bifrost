package keymanager

import utils.Logging
import scorex.crypto.hash.Sha256

import scala.io.Source
import scala.math.BigInt
import scala.util.{Failure, Success, Try}

/**
 * AMS Feb 2019:
 * Phrase translator class for translating given seed phrases
 * and UUID strings generated by java.util.UUID.randomUUID.toString
 *
 * Mnemonic seed phrase standard is given by the
 * Bitcoin Improvement Project 39 (BIP39) specification:
 * https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
 *
 * Expected input phrase length is 12, 15, 18, 21, or 24 words
 * corresponding to 128, 160, 192, 224, and 256 bits of entropy respectively
 *
 * Output is 12 word seed phrase produced by 128 bit entropy of a random UUID string
 *
 * Phrase list directory given by:
 * https://github.com/bitcoin/bips/tree/master/bip-0039
 *
 * @param wordList word list used for seed phrase
 */
class Bip39(wordList: List[String]) extends Logging {

  /*
   *  CS = ENT / 32
   *  MS = (ENT + CS) / 11
   *
   * |  ENT  | CS | ENT+CS |  MS  |
   * +-------+----+--------+------+
   * |  128  |  4 |   132  |  12  |
   * |  160  |  5 |   165  |  15  |
   * |  192  |  6 |   198  |  18  |
   * |  224  |  7 |   231  |  21  |
   * |  256  |  8 |   264  |  24  |
   *
   */

  val validPhraseLengths: List[Int] = List(12, 15, 18, 21, 24)
  val entMap: Map[Int, Int] = Map(12 -> 128, 15 -> 160, 18 -> 192, 21 -> 224, 24 -> 256)
  val chkMap: Map[Int, Int] = Map(12 -> 4, 15 -> 5, 18 -> 6, 21 -> 7, 24 -> 8)
  val endCSMap: Map[Int, Int] = Map(128 -> 4, 160 -> 5, 192 -> 6, 224 -> 7, 256 -> 8)
  val byteLen = 8
  val indexLen = 11

  def toBinaryIndex(i: Int): String = String.format("%11s", BigInt(i).toString(2)).replace(' ', '0')

  def toBinaryByte(b: Byte): String = String.format("%8s", BigInt(b & 0xff).toString(2)).replace(' ', '0')

  def hexToUuid(s: String): String =
    s.slice(0, 8) + "-" + s.slice(8, 12) + "-" + s.slice(12, 16) + "-" + s.slice(16, 20) + "-" + s.substring(20)

  /**
   * Checks if user input seedphrase is valid
   * @param phrase user input seedphrase
   * @return `true` if seedphrase is valid, false if seedphrase invalid
   */
  def phraseCheckSum(phrase: String): Boolean = {
    val phraseWords: List[String] = phrase.split(" ").toList
    val pl = phraseWords.length
    if (phraseWords.forall(word => wordList.contains(word)) && validPhraseLengths.contains(pl)) {
      val phraseBin = phraseWords.map(wordList.indexOf(_)).map(toBinaryIndex).mkString
      val phraseHashBin: List[String] = Sha256
        .hash(
          phraseBin.slice(0, entMap(pl)).grouped(byteLen).toArray map {
            Integer.parseInt(_, 2).toByte
          }
        )
        .map(toBinaryByte)
        .toList
      phraseBin.substring(entMap(pl)) == phraseHashBin.head.slice(0, chkMap(pl))
    } else {
      false
    }
  }

  /**
   * Translates valid seed phrase to a hex string
   * @param phrase user input seed phrase
   * @return hex string
   */
  def phraseToHex(phrase: String): String = {
    val phraseWords: List[String] = phrase.split(" ").toList
    val phraseBytes: Array[Byte] = phraseWords
      .map(wordList.indexOf(_))
      .map(toBinaryIndex)
      .mkString
      .slice(0, entMap(phraseWords.length))
      .grouped(byteLen)
      .toArray
      .map(Integer.parseInt(_, 2).toByte)
    phraseBytes.map("%02x" format _).mkString
  }

  /**
   * Produces a seed phrase from a UUID string
   * @param inputUuid generated by KeyFile.uuid
   * @return hex seed string, mnemonic seed phrase
   */
  def uuidSeedPhrase(inputUuid: String): (String, String) = {
    val seed = inputUuid.filterNot("-".toSet)
    val seedBytes: Array[Byte] = seed.grouped(2).toArray.map(Integer.parseInt(_, 16).toByte)
    val seedBin: Array[String] = seedBytes.map(toBinaryByte)
    val seedHashBin: Array[String] = Sha256.hash(seedBytes).map(toBinaryByte)
    val phrase = (seedBin.mkString("") + seedHashBin(0).slice(0, endCSMap(seedBin.mkString("").length)))
      .grouped(indexLen)
      .toArray
      .map(Integer.parseInt(_, 2))
      .map(wordList(_))
      .mkString(" ")
    (seed, phrase)
  }
}

object Bip39 {
  //fixme: JAA - this won't work on compilation since the resources move
  val phraseListDir = "src/main/resources/bip-0039/"

  val iso639_1_toFile: Map[String, String] = Map(
    "zh-hans" -> "chinese_simplified.txt",
    "zh-hant" -> "chinese_traditional.txt",
    "en"      -> "english.txt",
    "fr"      -> "french.txt",
    "it"      -> "italian.txt",
    "ja"      -> "japanese.txt",
    "ko"      -> "korean.txt",
    "es"      -> "spanish.txt"
  )

  def apply(phraseLanguage: String): Bip39 = {

    // open the specified file
    val wordList = Try(Source.fromFile(phraseListDir + iso639_1_toFile(phraseLanguage.toLowerCase))) match {
      case Success(file) => file.getLines().toList
      case Failure(ex)   => throw ex
    }

    if (verifyPhraseList(wordList: List[String], phraseLanguage)) {
      new Bip39(wordList)
    } else {
      throw new Error("The reference word lists cannot be verified. Aborting phrase conversion")
    }
  }

  /**
   * Verifies the wordlist for the given language by calculating the SHA2 hash
   * @return `true` if hash matches precalculated hash
   */
  def verifyPhraseList(wordList: List[String], phraseLanguage: String): Boolean = {
    val phraseLanguagesHash = Map(
      "chinese_simplified.txt"  -> "bfd683b91db88609fabad8968c7efe4bf69606bf5a49ac4a4ba5e355955670cb",
      "chinese_traditional.txt" -> "85b285c4e0e3eb1e52038e2cf4b4f8bba69fd814e1a09e063ce3609a1f67ad62",
      "english.txt"             -> "ad90bf3beb7b0eb7e5acd74727dc0da96e0a280a258354e7293fb7e211ac03db",
      "french.txt"              -> "9cbdaadbd3ce9cbaee1b360fce45e935b21e3e2c56d9fcd56b3398ced2371866",
      "italian.txt"             -> "80d2e90d7436603fd6e57cd9af6f839391e64beac1a3e015804f094fcc5ab24c",
      "japanese.txt"            -> "d9d1fde478cbeb45c06b93632a487eefa24f6533970f866ae81f136fbf810160",
      "korean.txt"              -> "f04f70b26cfef84474ff56582e798bcbc1a5572877d14c88ec66551272688c73",
      "spanish.txt"             -> "a556a26c6a5bb36db0fb7d8bf579cb7465fcaeec03957c0dda61b569962d9da5"
    )

    (phraseLanguagesHash(iso639_1_toFile(phraseLanguage.toLowerCase))
      == Sha256.hash(wordList.mkString).map("%02x" format _).mkString)
  }
}
