package co.topl.modifier.block

import co.topl.attestation.EvidenceProducer.syntax._
import co.topl.attestation.proof.{ Proof, SignatureCurve25519 }
import co.topl.attestation.proposition.{ Proposition, PublicKeyCurve25519Proposition }
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block._
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ ModifierId, NodeViewModifier }
import co.topl.nodeView.state.box.{ ArbitBox, Box, GenericBox }
import co.topl.utils.serialization.BifrostSerializer
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor, Json }
import scorex.crypto.hash.Blake2b256
import supertagged.@@
// fixme: JAA 0 2020.07.19 - why is protobuf still used here?
import serializer.BloomTopics

import scala.collection.BitSet

/**
 * A block is an atomic piece of data network participates are agreed on.
 *
 * A block has:
 * - transactional data: a sequence of transactions, where a transaction is an atomic state update.
 * Some metadata is possible as well(transactions Merkle tree root, state Merkle tree root etc).
 *
 * - consensus data to check whether block was generated by a right party in a right way. E.g.
 * "baseTarget" & "generatorSignature" fields in the Nxt block structure, nonce & difficulty in the
 * Bitcoin block structure.
 *
 * - a signature(s) of a block generator(s)
 *
 * - additional data: block structure version no, timestamp etc
 */
case class Block( parentId    : BlockId,
                  timestamp   : Timestamp,
                  forgerBox   : ArbitBox,
                  publicKey   : PublicKeyCurve25519Proposition,
                  signature   : SignatureCurve25519,
                  transactions: Seq[Block.TX],
                  version     : Version
                ) extends TransactionsCarryingPersistentNodeViewModifier[Block.TX] {

  type M = Block

  lazy val id: BlockId = ModifierId(Blake2b256(messageToSign))

  lazy val modifierTypeId: ModifierTypeId = Block.modifierTypeId

  lazy val serializer: BifrostSerializer[Block] = BlockSerializer

  lazy val messageToSign: Array[Byte] = {
    val noSigCopy = this.copy(signature = SignatureCurve25519.empty())
    serializer.toBytes(noSigCopy)
  }
}



object Block {

  type BlockId = ModifierId
  type Timestamp = Long
  type Version = Byte
  type TX = Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]]

  val blockIdLength: Int = NodeViewModifier.ModifierIdSize
  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = ModifierTypeId @@ (3: Byte)
  val signatureLength: Int = SignatureCurve25519.SignatureSize

  implicit val jsonEncoder: Encoder[Block] = { b: Block ⇒
    Map(
      "id" -> b.id.toString.asJson,
      "parentId" -> b.id.toString.asJson,
      "timestamp" -> b.timestamp.asJson,
      "generatorBox" -> b.forgerBox.asJson,
      "publicKey" -> b.publicKey.asJson,
      "signature" -> b.signature.asJson,
      "txs" -> b.transactions.asJson,
      "version" -> b.version.asJson,
      "blockSize" -> b.serializer.toBytes(b).length.asJson
      ).asJson
  }

  implicit val jsonDecoder: Decoder[Block] = (c: HCursor) =>
    for {
      parentId <- c.downField("parentId").as[ModifierId]
      timestamp <- c.downField("timestamp").as[Timestamp]
      generatorBox <- c.downField("generatorBox").as[ArbitBox]
      publicKey <- c.downField("publicKey").as[PublicKeyCurve25519Proposition]
      signature <- c.downField("signature").as[SignatureCurve25519]
      txsSeq <- c.downField("txs").as[Seq[TX]]
      version <- c.downField("version").as[Byte]
    } yield {
      Block(parentId, timestamp, generatorBox, publicKey, signature, txsSeq, version)
    }

  /**
   *
   * @param parentId
   * @param timestamp
   * @param txs
   * @param box
   * @param privateKey
   * @param version
   * @return
   */
  def create ( parentId  : BlockId,
               timestamp : Timestamp,
               txs       : Seq[TX],
               box       : ArbitBox,
               privateKey: PrivateKeyCurve25519,
               version   : Version
             ): Block = {

    // the owner of the generator box must be the key used to sign the block
    assert(box.evidence == privateKey.publicImage.generateEvidence)

    // generate block message (block with empty signature) to be signed
    val block = Block(parentId, timestamp, box, privateKey.publicImage, SignatureCurve25519.empty(), txs, version)

    // generate signature from the block message and private key
    val signature = privateKey.sign(block.messageToSign)

    // return a valid block with the signature attached
    block.copy(signature = signature)
  }

  /**
   *
   * @param txs
   * @return
   */
  def createBloom ( txs: Seq[TX] ): Array[Byte] = {
    val bloomBitSet = txs.foldLeft(BitSet.empty)(
      ( total, b ) =>
        b.bloomTopics match {
          case Some(e) => total ++ Bloom.calcBloom(e.head, e.tail)
          case None    => total
        }
      ).toSeq
    BloomTopics(bloomBitSet).toByteArray
  }
}
