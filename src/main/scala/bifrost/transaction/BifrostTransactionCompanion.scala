package bifrost.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import bifrost.contract._
import bifrost.transaction.box.{ContractBox, ContractBoxSerializer, ReputationBox}
import io.circe.{HCursor, Json, ParsingFailure}
import io.circe.optics.JsonPath._
import io.circe.parser._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import cats.syntax.either._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.collection.immutable.HashMap
import scala.util.Try

object BifrostTransactionCompanion extends Serializer[BifrostTransaction] {

  override def toBytes(m: BifrostTransaction): Array[Byte] = m match {
    case c: ContractTransaction => ContractTransactionCompanion.toBytes(c)
    case p: TransferTransaction => TransferTransactionCompanion.toBytes(p)
    case r: ProfileTransaction => ProfileTransactionCompanion.toBytes(r)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.slice(0, Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,Ints.BYTES + typeLength))

    typeStr match {
      case "ContractTransaction" => ContractTransactionCompanion.parseBytes(bytes).get.asInstanceOf[BifrostTransaction]
      case "TransferTransaction" => TransferTransactionCompanion.parseBytes(bytes).get.asInstanceOf[BifrostTransaction]
      case "ProfileTransaction" => ProfileTransactionCompanion.parseBytes(bytes).get
    }
  }

}

object ContractTransactionCompanion extends Serializer[ContractTransaction] {

  override def toBytes(m: ContractTransaction): Array[Byte] = {
    val typeBytes = "ContractTransaction".getBytes

    Ints.toByteArray(typeBytes.length) ++
      typeBytes ++
      (m match {
        case cc: ContractCreation => ContractCreationCompanion.toBytes(cc)
        case cme: ContractMethodExecution => ContractMethodExecutionCompanion.toBytes(cme)
        case ccomp: ContractCompletion => ContractCompletionCompanion.toBytes(ccomp)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,Ints.BYTES + typeLength))

    /* Grab the rest of the bytes, which should begin similarly (with sub-type) */
    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.take(Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES,  Ints.BYTES + newTypeLength))

      newTypeStr match {
        case "ContractCreation" => ContractCreationCompanion.parseBytes(newBytes).get
        case "ContractMethodExecution" => ContractMethodExecutionCompanion.parseBytes(newBytes).get
        case "ContractCompletion" => ContractCompletionCompanion.parseBytes(newBytes).get
      }
  }
}

object TransferTransactionCompanion extends Serializer[TransferTransaction] {

  override def toBytes(m: TransferTransaction): Array[Byte] = {
    val typeBytes = "TransferTransaction".getBytes

    Ints.toByteArray(typeBytes.length) ++
      typeBytes ++
      (m match {
        case sc: PolyTransfer => PolyTransferCompanion.toBytes(sc)
        case ac: ArbitTransfer => ArbitTransferCompanion.toBytes(ac)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.slice(0, Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES, Ints.BYTES + newTypeLength))

    newTypeStr match {
      case "PolyTransfer" => PolyTransferCompanion.parseBytes(newBytes).get.asInstanceOf[TransferTransaction]
      case "ArbitTransfer" => ArbitTransferCompanion.parseBytes(newBytes).get.asInstanceOf[TransferTransaction]
    }
  }
}

object ProfileTransactionCompanion extends Serializer[ProfileTransaction] {
  override def toBytes(m: ProfileTransaction): Array[Byte] = {
    val typeBytes = "ProfileTransaction".getBytes

    Bytes.concat(
      Ints.toByteArray(typeBytes.length), typeBytes,
      m.json.toString().getBytes()
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ProfileTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val json: Json = parse(new String(bytesWithoutType)).getOrElse(Json.Null)
    val cursor: HCursor = json.hcursor

    val from = PublicKey25519Proposition(Base58.decode(root.from.string.getOption(json).get).get)
    val fee = root.fee.long.getOption(json).get
    val timestamp = root.timestamp.long.getOption(json).get
    val signature = Signature25519(Base58.decode(root.signature.string.getOption(json).get).get)
    val keyValues: Map[String, String] = cursor.downField("keyValues").as[Map[String, String]].getOrElse(Map())

    ProfileTransaction(from, signature, keyValues, fee, timestamp)
  }
}

object ContractCreationCompanion extends Serializer[ContractCreation] {

  override def toBytes(m: ContractCreation): Array[Byte] = {
    val typeBytes = "ContractCreation".getBytes

    val agreementBytes = AgreementCompanion.toBytes(m.agreement)

    // TODO this might need a nonce
    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(m.fee),
      Longs.toByteArray(m.timestamp),
      Longs.toByteArray(agreementBytes.length),
      Ints.toByteArray(m.signatures.length),
      Ints.toByteArray(m.parties.length),
      m.parties.foldLeft(Array[Byte]())((a,b) => a ++ (b._1 match {
        case Role.Producer => Ints.toByteArray(0)
        case Role.Investor => Ints.toByteArray(1)
        case Role.Hub => Ints.toByteArray(2)
      })),
      agreementBytes,
      m.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
      m.parties.foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractCreation] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val Array(fee: Long, timestamp: Long, agreementLength: Long) = (0 until 3).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i*Longs.BYTES, (i + 1)*Longs.BYTES))
    }.toArray

    numReadBytes = 3*Longs.BYTES

    val Array(sigLength: Int, partiesLength: Int, role1: Int, role2: Int, role3: Int) = (0 until 5).map { i =>
      Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + i*Ints.BYTES, numReadBytes + (i + 1)*Ints.BYTES))
    }.toArray

    numReadBytes += 5*Ints.BYTES

    val agreement = AgreementCompanion.parseBytes(
      bytesWithoutType.slice(
        numReadBytes,
        numReadBytes + agreementLength.toInt
      )
    ).get

    numReadBytes += agreementLength.toInt

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytesWithoutType.slice(numReadBytes + i * Curve25519.SignatureLength, numReadBytes + (i + 1) * Curve25519.SignatureLength))
    }

    numReadBytes += sigLength * Curve25519.SignatureLength

    val parties = (0 until partiesLength) map { i =>
      val pk = bytesWithoutType.slice(numReadBytes + i * Curve25519.KeyLength, numReadBytes + (i + 1) * Curve25519.KeyLength )
      PublicKey25519Proposition(pk)
    }

    val roleTypes = IndexedSeq(role1, role2, role3).map {
      case 0 => Role.Producer
      case 1 => Role.Investor
      case 2 => Role.Hub
    }

    ContractCreation(agreement, roleTypes.zip(parties), signatures, fee, timestamp)
  }

}

object ContractMethodExecutionCompanion extends Serializer[ContractMethodExecution] {

  override def toBytes(cme: ContractMethodExecution): Array[Byte] = {
    val typeBytes = "ContractMethodExecution".getBytes

    // TODO this might need a nonce
      Bytes.concat(
        /* First two arguments MUST STAY */
        Ints.toByteArray(typeBytes.length),
        typeBytes,
        Longs.toByteArray(cme.fee),
        Longs.toByteArray(cme.timestamp),
        Ints.toByteArray(cme.methodName.getBytes.length),
        Ints.toByteArray(cme.parameters.noSpaces.getBytes.length),
        cme.party._1 match {
          case Role.Producer => Ints.toByteArray(0)
          case Role.Investor => Ints.toByteArray(1)
          case Role.Hub => Ints.toByteArray(2)
        },
        cme.party._2.pubKeyBytes,
        cme.methodName.getBytes,
        cme.parameters.noSpaces.getBytes,
        cme.signature.bytes,
        cme.contractBox.bytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractMethodExecution] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))

    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i*Longs.BYTES, (i + 1)*Longs.BYTES))
    }.toArray

    numReadBytes = 2*Longs.BYTES

    val Array(methodNameLength: Int, parameterJsonLength: Int, roleInt: Int) = (0 until 3).map { i =>
      Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + i*Ints.BYTES, numReadBytes + (i + 1)*Ints.BYTES))
    }.toArray

    numReadBytes += 3*Ints.BYTES

    val party: PublicKey25519Proposition = PublicKey25519Proposition(bytesWithoutType.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val methodName = new String(bytesWithoutType.slice(numReadBytes, numReadBytes + methodNameLength))

    numReadBytes += methodNameLength

    val parameters: Json = parse(new String(bytesWithoutType.slice(numReadBytes, numReadBytes + parameterJsonLength))) match {
      case Left(f) => throw f
      case Right(j: Json) => j
    }

    numReadBytes += parameterJsonLength

    val signature = Signature25519(bytesWithoutType.slice(numReadBytes, numReadBytes + Signature25519.SignatureSize))

    numReadBytes += Signature25519.SignatureSize

    val contractBox: ContractBox = ContractBoxSerializer.parseBytes(bytesWithoutType.slice(numReadBytes, bytesWithoutType.length)).get

    val role = roleInt match {
      case 0 => Role.Producer
      case 1 => Role.Investor
      case 2 => Role.Hub
    }

    ContractMethodExecution(contractBox, role -> party, methodName, parameters, signature, fee, timestamp)
  }

}

object ContractCompletionCompanion extends Serializer[ContractCompletion] {

  override def toBytes(cc: ContractCompletion): Array[Byte] = {
    val typeBytes = "ContractCompletion".getBytes

    // TODO this might need a nonce
    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(cc.fee),
      Longs.toByteArray(cc.timestamp),
      Ints.toByteArray(cc.signatures.length),
      Ints.toByteArray(cc.parties.length),
      Ints.toByteArray(cc.reputation.length),
      cc.parties.foldLeft(Array[Byte]())((a, b) => a ++ (b._1 match {
        case Role.Producer => Ints.toByteArray(0)
        case Role.Investor => Ints.toByteArray(1)
        case Role.Hub => Ints.toByteArray(2)
      })),
      cc.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
      cc.parties.foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes),
      cc.reputation.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)),
      cc.contractBox.bytes
    )
  }

  //noinspection ScalaStyle
  override def parseBytes(bytes: Array[Byte]): Try[ContractCompletion] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i*Longs.BYTES, (i + 1)*Longs.BYTES))
    }.toArray

    numReadBytes = 2*Longs.BYTES

    val Array(sigLength: Int, partiesLength: Int, reputationLength: Int, role1: Int, role2: Int, role3: Int) = (0 until 6).map { i =>
      Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + i*Ints.BYTES, numReadBytes + (i + 1)*Ints.BYTES))
    }.toArray

    numReadBytes += 6*Ints.BYTES

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytesWithoutType.slice(numReadBytes + i * Curve25519.SignatureLength, numReadBytes + (i + 1) * Curve25519.SignatureLength))
    }

    numReadBytes += sigLength * Curve25519.SignatureLength

    val parties = (0 until partiesLength) map { i =>
      val pk = bytesWithoutType.slice(numReadBytes + i * Curve25519.KeyLength, numReadBytes + (i + 1) * Curve25519.KeyLength )
      PublicKey25519Proposition(pk)
    }

    numReadBytes += partiesLength * Curve25519.KeyLength

    val reputation: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until reputationLength) map { i =>
      val proposition = PublicKey25519Proposition(bytesWithoutType.slice(
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES),
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES) + Constants25519.PubKeyLength
      ))

      val nonce = Longs.fromByteArray(bytesWithoutType.slice(
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES) + Constants25519.PubKeyLength,
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES) + Constants25519.PubKeyLength + Longs.BYTES
      ))

      (proposition, nonce)
    }

    val contractBox: ContractBox = ContractBoxSerializer.parseBytes(bytesWithoutType.slice(numReadBytes, bytesWithoutType.length)).get

    val roleTypes = IndexedSeq(role1, role2, role3).map {
      case 0 => Role.Producer
      case 1 => Role.Investor
      case 2 => Role.Hub
    }

    ContractCompletion(contractBox, reputation, roleTypes.zip(parties), signatures, fee, timestamp)
  }

}

object AgreementCompanion extends Serializer[Agreement] {

  override def toBytes(a: Agreement): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(a.contractExpirationTime),
      Longs.toByteArray(a.contractEffectiveTime),
      Longs.toByteArray(a.terms.json.noSpaces.getBytes.length),
      a.terms.json.noSpaces.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[Agreement] = Try {

    val Array(contractExpirationTime: Long, contractEffectiveTime: Long, termsLength: Long) = (0 until 3).map { i =>
      Longs.fromByteArray(bytes.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    var numBytesRead = 3*Longs.BYTES

    val termsMap: Map[String, Json] = parse(new String(
      bytes.slice(numBytesRead, numBytesRead + termsLength.toInt)
    )) match {
      case Left(x) => new HashMap[String, Json]()
      case Right(x) => x.asObject.get.toMap
    }

    val fulfilmentMap = termsMap("fulfilment").asObject.get.toMap
    val shareMap = termsMap("share").asObject.get.toMap

    val terms = new AgreementTerms(
      termsMap("pledge").asNumber.get.toLong.get,
      BigDecimal(termsMap("xrate").asString.get),
      shareMap("functionType").asString.get match {
        case "PiecewiseLinearMultiple" => new PiecewiseLinearMultiple(
          shareMap("points").as[Seq[(Double,(Double, Double, Double))]].right.get
        )
      },
      fulfilmentMap("functionType").asString.get match {
        case "PiecewiseLinearSingle" => new PiecewiseLinearSingle(
          fulfilmentMap("points").as[Seq[(Long, Double)]].right.get)
      }
    )

    Agreement(terms, contractEffectiveTime, contractExpirationTime)
  }
}

trait TransferSerializer {
  def transferToBytes(tx: TransferTransaction, txType: String): Array[Byte] = {
    val typeBytes = txType.getBytes

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(tx.fee),
      Longs.toByteArray(tx.timestamp),
      Ints.toByteArray(tx.signatures.length),
      Ints.toByteArray(tx.from.length),
      Ints.toByteArray(tx.to.length),
      tx.signatures.foldLeft(Array[Byte]())((a,b) => a ++ b.bytes),
      tx.from.foldLeft(Array[Byte]())((a,b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)),
      tx.to.foldLeft(Array[Byte]())((a,b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
    )
  }

  def parametersParseBytes(bytes: Array[Byte]): (IndexedSeq[(PublicKey25519Proposition, Long)],
                                                 IndexedSeq[(PublicKey25519Proposition, Long)],
                                                 IndexedSeq[Signature25519], Long, Long) = {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))

    var numBytesRead = Ints.BYTES + typeLength

    val fee = Longs.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Longs.BYTES))
    val timestamp = Longs.fromByteArray(bytes.slice(numBytesRead + Longs.BYTES, numBytesRead + 2*Longs.BYTES))
    val sigLength = Ints.fromByteArray(bytes.slice( numBytesRead + 2*Longs.BYTES, numBytesRead + 2*Longs.BYTES + Ints.BYTES))

    numBytesRead += 2*Longs.BYTES + Ints.BYTES

    val fromLength = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    val toLength = Ints.fromByteArray(bytes.slice(numBytesRead + Ints.BYTES, numBytesRead + 2*Ints.BYTES))

    numBytesRead += 2*Ints.BYTES

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytes.slice(numBytesRead + i * Curve25519.SignatureLength, numBytesRead + (i + 1) * Curve25519.SignatureLength))
    }

    numBytesRead += sigLength*Curve25519.SignatureLength

    val elementLength = Longs.BYTES + Curve25519.KeyLength

    val from = (0 until fromLength) map { i =>
      val pk = bytes.slice(numBytesRead + i*elementLength, numBytesRead + (i + 1)*elementLength - Longs.BYTES)
      val nonce = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1)*elementLength - Longs.BYTES, numBytesRead + (i + 1)*elementLength)
      )
      (PublicKey25519Proposition(pk), nonce)
    }

    numBytesRead += fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = bytes.slice(numBytesRead + i*elementLength, numBytesRead + (i + 1)*elementLength - Longs.BYTES)
      val v = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1)*elementLength - Longs.BYTES, numBytesRead + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), v)
    }
    (from, to, signatures, fee, timestamp)
  }
}

object PolyTransferCompanion extends Serializer[PolyTransfer] with TransferSerializer {

  override def toBytes(sc: PolyTransfer): Array[Byte] = {
    transferToBytes(sc, "PolyTransfer")
  }

  override def parseBytes(bytes: Array[Byte]): Try[PolyTransfer] = Try {
    val params = parametersParseBytes(bytes)
    PolyTransfer(params._1, params._2, params._3, params._4, params._5)
  }
}

object ArbitTransferCompanion extends Serializer[ArbitTransfer] with TransferSerializer {

  override def toBytes(ac: ArbitTransfer): Array[Byte] = {
    transferToBytes(ac, "ArbitTransfer")
  }

  override def parseBytes(bytes: Array[Byte]): Try[ArbitTransfer] = Try {
    val params = parametersParseBytes(bytes)
    ArbitTransfer(params._1, params._2, params._3, params._4, params._5)
  }
}