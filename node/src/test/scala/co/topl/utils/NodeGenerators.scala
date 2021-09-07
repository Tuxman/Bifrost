package co.topl.utils

import co.topl.attestation._
import co.topl.attestation.keyManagement._
import co.topl.consensus.TestGenesis
import co.topl.db.LDBVersionedStore
import co.topl.modifier.block.Block
import co.topl.modifier.box.Box.identifier
import co.topl.modifier.box._
import co.topl.modifier.transaction.ArbitTransfer.Validation.InvalidArbitTransfer
import co.topl.modifier.transaction.AssetTransfer.Validation.InvalidAssetTransfer
import co.topl.modifier.transaction.PolyTransfer.Validation.InvalidPolyTransfer
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction.builder.BoxPickingStrategy
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.modifier.{transaction, ModifierId}
import co.topl.nodeView.history.{BlockProcessor, History, Storage}
import co.topl.nodeView.state.State
import co.topl.settings.{AppSettings, StartupOpts, Version}
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.StringDataTypes.Latin1Data
import org.scalacheck.Gen
import org.scalatest.Suite
import co.topl.modifier.transaction.builder.implicits._

import java.io.File
import java.nio.file.Files
import scala.collection.immutable.ListMap
import scala.util.Random

trait NodeGenerators extends CommonGenerators with KeyFileTestHelper {
  self: Suite =>

  private val settingsFilename = "node/src/test/resources/test.conf"

  lazy val settings: AppSettings = {
    val s = AppSettings.read(StartupOpts(Some(settingsFilename)))._1
    s.copy(
      application = s.application.copy(
        dataDir = Some(Files.createTempDirectory("bifrost-test-data").toString)
      )
    )
  }

  lazy val versionGen: Gen[Version] = for {
    first  <- Gen.choose(0: Byte, Byte.MaxValue)
    second <- Gen.choose(0: Byte, Byte.MaxValue)
    third  <- Gen.choose(0: Byte, Byte.MaxValue)
  } yield new Version(first, second, third)

  lazy val genesisBlock: Block =
    TestGenesis(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, settings).getGenesisBlock.get._1

  def genesisBlockId: ModifierId = genesisBlock.id

  def generateHistory(genesisBlock: Block = genesisBlock): History = {
    val dataDir = s"/tmp/bifrost/test-data/test-${Random.nextInt(10000000)}"

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LDBVersionedStore(iFile, 100)

    val storage = new Storage(blockStorage, settings.application.cacheExpire, settings.application.cacheSize)
    //we don't care about validation here
    val validators = Seq()

    var history = new History(storage, BlockProcessor(1024), validators)

    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

  def genesisState(settings: AppSettings, genesisBlockWithVersion: Block = genesisBlock): State = {
    History.readOrGenerate(settings).append(genesisBlock)
    State.genesisState(settings, Seq(genesisBlockWithVersion))
  }

  lazy val genesisState: State = genesisState(settings)

  lazy val validBifrostTransactionSeqGen: Gen[Seq[TX]] = for {
    seqLen <- positiveMediumIntGen
  } yield 0 until seqLen map { _ =>
    val g: Gen[TX] = sampleUntilNonEmpty(
      Gen.oneOf(
        validPolyTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, genesisState),
        validArbitTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, genesisState),
        validAssetTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, genesisState, minting = true)
      )
    )
    sampleUntilNonEmpty(g)
  }

  def validPolyTransferCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[PolyTransfer[PublicKeyPropositionCurve25519]] = {

    val availablePolys = sumBoxes(collectBoxes(keyRing.addresses, state), "PolyBox")
    val (sender, poly) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, poly.longValue() - 1))) - fee)

    val recipients = {
      val address: Address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, polyAmount))
    }

    val rawTx =
      transaction.builder
        .buildTransfer[SimpleValue, InvalidPolyTransfer, PolyTransfer[
          PublicKeyPropositionCurve25519
        ], BoxPickingStrategy.All](
          IndexedSeq(sender),
          recipients,
          state,
          sender,
          fee
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validPolyTransferThresholdCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    props:   Set[ThresholdPropositionCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[PolyTransfer[ThresholdPropositionCurve25519]] = {

    val addresses = props.map(_.address)
    val addressesToPropMap = props.map(prop => (prop.address, prop)).toMap

    val availablePolys: Seq[(Address, Int128)] = sumBoxes(collectBoxes(addresses, state), "PolyBox")
    val (sender, poly): (Address, Int128) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, poly.longValue() - 1))) - fee)

    val recipients = {
      val address: Address =
        addresses.filterNot(_ == sender).toSeq(Random.nextInt(addresses.size - 1))
      IndexedSeq((address, polyAmount))
    }

    val rawTx =
      transaction.builder
        .buildTransfer[SimpleValue, InvalidPolyTransfer, PolyTransfer[
          ThresholdPropositionCurve25519
        ], BoxPickingStrategy.All](
          IndexedSeq(sender),
          recipients,
          state,
          sender,
          fee
        )
        .getOrThrow()

    val signatures = keyRing.generateAttestation(keyRing.addresses)(rawTx.messageToSign).values.toSet
    val thresholdSignature = ThresholdSignatureCurve25519(signatures)
    val attestation = ListMap(addressesToPropMap(sender) -> thresholdSignature)

    rawTx.copy(attestation = attestation)
  }

  def validPolyTransferEd25519Gen(
    keyRing: KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[PolyTransfer[PublicKeyPropositionEd25519]] = {

    val availablePolys = sumBoxes(collectBoxes(keyRing.addresses, state), "PolyBox")
    val (sender, poly) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, poly.longValue() - 1))) - fee)

    val recipients = {
      val address: Address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, polyAmount))
    }

    val rawTx = transaction.builder
      .buildTransfer[SimpleValue, InvalidPolyTransfer, PolyTransfer[
        PublicKeyPropositionEd25519
      ], BoxPickingStrategy.All](
        IndexedSeq(sender),
        recipients,
        state,
        sender,
        fee
      )
      .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validPolyTransferGen(
    keyRingCurve25519:        KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    keyRingEd25519:           KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    propsThresholdCurve25519: Set[ThresholdPropositionCurve25519],
    state:                    State,
    fee:                      Long = 1L
  ): Gen[PolyTransfer[_ <: Proposition]] =
    Gen.oneOf(
      validPolyTransferCurve25519Gen(keyRingCurve25519, state, fee),
      validPolyTransferThresholdCurve25519Gen(keyRingCurve25519, propsThresholdCurve25519, state, fee),
      validPolyTransferEd25519Gen(keyRingEd25519, state, fee)
    )

  def validArbitTransferCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[ArbitTransfer[PublicKeyPropositionCurve25519]] = {

    val availableArbits = sumBoxes(collectBoxes(keyRing.addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, arbit.longValue() - 1))) - fee)

    val recipients = {
      val address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, arbitAmount))
    }

    val rawTx =
      transaction.builder
        .buildTransfer[SimpleValue, InvalidArbitTransfer, ArbitTransfer[
          PublicKeyPropositionCurve25519
        ], BoxPickingStrategy.All](
          IndexedSeq(sender),
          recipients,
          state,
          sender,
          fee
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validArbitTransferThresholdCurve25519Gen(
    keyRing:      KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    propositions: Set[ThresholdPropositionCurve25519],
    state:        State,
    fee:          Long = 1L
  ): Gen[ArbitTransfer[ThresholdPropositionCurve25519]] = {

    val addresses = propositions.map(_.address)
    val addressesToPropMap = propositions.map(prop => (prop.address, prop)).toMap

    val availableArbits = sumBoxes(collectBoxes(addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, arbit.longValue() - 1))) - fee)

    val recipients = {
      val address: Address =
        addresses.filterNot(_ == sender).toSeq(Random.nextInt(addresses.size - 1))
      IndexedSeq((address, arbitAmount))
    }

    val rawTx =
      transaction.builder
        .buildTransfer[SimpleValue, InvalidArbitTransfer, ArbitTransfer[
          PublicKeyPropositionCurve25519
        ], BoxPickingStrategy.All](
          IndexedSeq(sender),
          recipients,
          state,
          sender,
          fee
        )
        .getOrThrow()

    val signatures = keyRing.generateAttestation(keyRing.addresses)(rawTx.messageToSign).values.toSet
    val thresholdSignature = ThresholdSignatureCurve25519(signatures)
    val attestation = ListMap(addressesToPropMap(sender) -> thresholdSignature)

    rawTx.copy(attestation = attestation)
  }

  def validArbitTransferEd25519Gen(
    keyRing: KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[ArbitTransfer[PublicKeyPropositionEd25519]] = {

    val availableArbits = sumBoxes(collectBoxes(keyRing.addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, arbit.longValue() - 1))) - fee)

    val recipients = {
      val address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, arbitAmount))
    }

    val rawTx =
      transaction.builder
        .buildTransfer[SimpleValue, InvalidArbitTransfer, ArbitTransfer[
          PublicKeyPropositionEd25519
        ], BoxPickingStrategy.All](
          IndexedSeq(sender),
          recipients,
          state,
          sender,
          fee
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validArbitTransferGen(
    keyRingCurve25519:        KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    keyRingEd25519:           KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    propsThresholdCurve25519: Set[ThresholdPropositionCurve25519],
    state:                    State,
    fee:                      Long = 1L
  ): Gen[ArbitTransfer[_ <: Proposition]] =
    Gen.oneOf(
      validArbitTransferCurve25519Gen(keyRingCurve25519, state, fee),
      validArbitTransferThresholdCurve25519Gen(keyRingCurve25519, propsThresholdCurve25519, state, fee),
      validArbitTransferEd25519Gen(keyRingEd25519, state, fee)
    )

  def validAssetTransferCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L,
    minting: Boolean = false
  ): Gen[AssetTransfer[PublicKeyPropositionCurve25519]] = {
    val sender = keyRing.addresses.head
    val asset = AssetValue(1, AssetCode(1: Byte, sender, Latin1Data.unsafe("test")), SecurityRoot.empty)
    val recipients = IndexedSeq((sender, asset))

    val rawTx =
      transaction.builder
        .buildTransfer[AssetValue, InvalidAssetTransfer, AssetTransfer[
          PublicKeyPropositionCurve25519
        ], BoxPickingStrategy.All](
          IndexedSeq(sender),
          recipients,
          state,
          sender,
          fee,
          minting = minting
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validAssetTransferThresholdCurve25519Gen(
    keyRing:      KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    propositions: Set[ThresholdPropositionCurve25519],
    state:        State,
    fee:          Long = 1L,
    minting:      Boolean = false
  ): Gen[AssetTransfer[ThresholdPropositionCurve25519]] = {

    val addresses = propositions.map(_.address)
    val addressesToPropMap = propositions.map(prop => (prop.address, prop)).toMap

    val sender = addresses.head
    val asset = AssetValue(1, AssetCode(1: Byte, sender, Latin1Data.unsafe("test")), SecurityRoot.empty)
    val recipients = IndexedSeq((sender, asset))

    val rawTx =
      transaction.builder
        .buildTransfer[AssetValue, InvalidAssetTransfer, AssetTransfer[
          PublicKeyPropositionCurve25519
        ], BoxPickingStrategy.All](
          IndexedSeq(sender),
          recipients,
          state,
          sender,
          fee,
          minting = minting
        )
        .getOrThrow()

    val signatures = keyRing.generateAttestation(keyRing.addresses)(rawTx.messageToSign).values.toSet
    val thresholdSignature = ThresholdSignatureCurve25519(signatures)
    val attestation = ListMap(addressesToPropMap(sender) -> thresholdSignature)

    rawTx.copy(attestation = attestation)
  }

  def validAssetTransferEd25519Gen(
    keyRing: KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:   State,
    fee:     Long = 1L,
    minting: Boolean = false
  ): Gen[AssetTransfer[PublicKeyPropositionEd25519]] = {
    val sender = keyRing.addresses.head
    val asset = AssetValue(1, AssetCode(1: Byte, sender, Latin1Data.unsafe("test")), SecurityRoot.empty)
    val recipients = IndexedSeq((sender, asset))

    val rawTx =
      transaction.builder
        .buildTransfer[AssetValue, InvalidAssetTransfer, AssetTransfer[
          PublicKeyPropositionEd25519
        ], BoxPickingStrategy.All](
          IndexedSeq(sender),
          recipients,
          state,
          sender,
          fee,
          minting = minting
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validAssetTransferGen(
    keyRingCurve25519:        KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    keyRingEd25519:           KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    propsThresholdCurve25519: Set[ThresholdPropositionCurve25519],
    state:                    State,
    fee:                      Long = 1L,
    minting:                  Boolean = false
  ): Gen[AssetTransfer[_ <: Proposition]] =
    Gen.oneOf(
      validAssetTransferCurve25519Gen(keyRingCurve25519, state, fee, minting),
      validAssetTransferThresholdCurve25519Gen(keyRingCurve25519, propsThresholdCurve25519, state, fee, minting),
      validAssetTransferEd25519Gen(keyRingEd25519, state, fee, minting)
    )

  def collectBoxes(addresses: Set[Address], state: State): Seq[TokenBox[TokenValueHolder]] =
    addresses.flatMap(address => state.getTokenBoxes(address)).flatten.toSeq

  def sumBoxes(boxes: Seq[TokenBox[TokenValueHolder]], tokenType: String): Seq[(Address, Int128)] = {
    val boxesByOwner = boxes.groupBy(_.evidence)
    val ownerQuantities = boxesByOwner.map { case (evidence, boxes) =>
      Address(evidence) -> boxes
        .filter(identifier(_).typeString == tokenType)
        .map(_.value.quantity)
        .sum
    }.toSeq
    ownerQuantities.filter(_._2 > 0)
  }
}
