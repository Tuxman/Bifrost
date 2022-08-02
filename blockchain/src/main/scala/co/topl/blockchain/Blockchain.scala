package co.topl.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, Sink, Source}
import akka.util.ByteString
import cats.data.{OptionT, Validated}
import cats.effect._
import cats.implicits._
import cats.{MonadThrow, Parallel}
import co.topl.algebras.{Store, UnsafeResource}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.BlockHeaderV2Ops
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.grpc.ToplGrpc
import co.topl.ledger.algebras._
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.models._
import co.topl.networking.blockchain._
import co.topl.networking.p2p.{ConnectedPeer, DisconnectedPeer, LocalPeer}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import BlockchainPeerHandler.monoidBlockchainPeerHandler
import scala.util.Random

object Blockchain {

  /**
   * A forever-running program which traverses epochs and the slots within the epochs
   */
  def run[F[_]: Parallel: MonadThrow: Logger: Async: FToFuture](
    mint:                          Option[PerpetualBlockMintAlgebra[F]],
    slotDataStore:                 Store[F, TypedIdentifier, SlotData],
    headerStore:                   Store[F, TypedIdentifier, BlockHeaderV2],
    bodyStore:                     Store[F, TypedIdentifier, BlockBodyV2],
    transactionStore:              Store[F, TypedIdentifier, Transaction],
    _localChain:                   LocalChainAlgebra[F],
    blockIdTree:                   ParentChildTree[F, TypedIdentifier],
    blockHeights:                  EventSourcedState[F, Long => F[Option[TypedIdentifier]]],
    headerValidation:              BlockHeaderValidationAlgebra[F],
    transactionSyntaxValidation:   TransactionSyntaxValidationAlgebra[F],
    transactionSemanticValidation: TransactionSemanticValidationAlgebra[F],
    bodySyntaxValidation:          BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:        BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation:   BodyAuthorizationValidationAlgebra[F],
    _mempool:                      MempoolAlgebra[F],
    ed25519VrfResource:            UnsafeResource[F, Ed25519VRF],
    localPeer:                     LocalPeer,
    remotePeers:                   Source[DisconnectedPeer, _],
    peerFlowModifier: (
      ConnectedPeer,
      Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]]
    ) => Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]],
    rpcHost:         String,
    rpcPort:         Int
  )(implicit system: ActorSystem[_], random: Random): F[Unit] =
    for {
      (localChain, _localBlockAdoptionsSource) <- LocalChainBroadcaster.make(_localChain)
      localBlockAdoptionsSource <- _localBlockAdoptionsSource.toMat(BroadcastHub.sink)(Keep.right).liftTo[F]
      (mempool, _localTransactionAdoptionsSource) <- MempoolBroadcaster.make(_mempool)
      localTransactionAdoptionsSource <- _localTransactionAdoptionsSource.toMat(BroadcastHub.sink)(Keep.right).liftTo[F]
      clientHandler =
        List(
          BlockchainPeerHandler.ChainSynchronizer.make[F](
            localChain,
            headerValidation,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            slotDataStore,
            headerStore,
            bodyStore,
            transactionStore,
            blockIdTree
          ),
          BlockchainPeerHandler.FetchMempool.make(
            transactionSyntaxValidation,
            transactionStore,
            mempool
          ),
          BlockchainPeerHandler.CommonAncestorSearch.make(
            id =>
              OptionT(
                localChain.head
                  .map(_.slotId.blockId)
                  .flatMap(blockHeights.useStateAt(_)(_.apply(id)))
              ).toRight(new IllegalStateException("Unable to determine block height tree")).rethrowT,
            () => localChain.head.map(_.height),
            slotDataStore
          )
        ).combineAll
      peerServer <- BlockchainPeerServer.FromStores.make(
        slotDataStore,
        headerStore,
        bodyStore,
        transactionStore,
        blockHeights,
        localChain,
        localBlockAdoptionsSource
          .tapAsyncF(1)(id => Logger[F].info(show"Broadcasting block id=$id to peers"))
          .pure[F],
        localTransactionAdoptionsSource
          .tapAsyncF(1)(id => Logger[F].info(show"Broadcasting transaction id=$id to peers"))
          .pure[F]
      )
      (p2pServer, p2pFiber) <- BlockchainNetwork
        .make[F](
          localPeer.localAddress.getHostName,
          localPeer.localAddress.getPort,
          localPeer,
          remotePeers,
          clientHandler,
          peerServer,
          peerFlowModifier
        )
      mintedBlockStream <- mint.fold(Source.never[BlockV2].pure[F])(_.blocks)
      rpcInterpreter <- ToplRpcServer.make(
        headerStore,
        transactionStore,
        mempool,
        transactionSyntaxValidation,
        transactionSemanticValidation,
        localChain
      )
      rpcServer = ToplGrpc.Server.serve(rpcHost, rpcPort, rpcInterpreter)
      mintedBlockStreamCompletionFuture =
        mintedBlockStream
          .tapAsyncF(1)(block => Logger[F].info(show"Minted header=${block.headerV2} body=${block.blockBodyV2}"))
          .tapAsyncF(1)(block =>
            blockIdTree.associate(block.headerV2.id, block.headerV2.parentHeaderId) >>
            ed25519VrfResource.use(implicit e => slotDataStore.put(block.headerV2.id, block.headerV2.slotData)) >>
            headerStore.put(block.headerV2.id, block.headerV2) >>
            bodyStore.put(block.headerV2.id, block.blockBodyV2)
          )
          .mapAsyncF(1)(block => ed25519VrfResource.use(implicit ed25519Vrf => block.headerV2.slotData.pure[F]))
          .map(Validated.Valid(_))
          .tapAsyncF(1)(localChain.adopt)
          .toMat(Sink.ignore)(Keep.right)
          .liftTo[F]
      _ <- rpcServer.use(binding =>
        Logger[F].info(s"RPC Server bound at ${binding.localAddress}") >>
        Async[F].fromFuture(mintedBlockStreamCompletionFuture) >>
        p2pFiber.join
      )
    } yield ()

}
