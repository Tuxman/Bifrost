package co.topl.genusLibrary.orientDb

import cats.implicits._
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.DbFixtureUtil
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, NodeBlockFetcherAlgebra}
import co.topl.genusLibrary.interpreter.GraphBlockUpdater
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators._
import co.topl.node.models.FullBlockBody
import fs2.Stream

import java.util.concurrent.TimeUnit
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters.IterableHasAsScala

class GraphBlockUpdaterFixtureTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMaxSize(3)
      .withMinSuccessfulTests(5)

  override def munitTimeout: Duration =
    new FiniteDuration(10, TimeUnit.SECONDS)

  orientDbFixture.test("Insert and remove genesis block") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    PropF.forAllF { (blockHeader: BlockHeader, blockBody: FullBlockBody) =>
      withMock {
        val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
        val blockFetcher = mock[BlockFetcherAlgebra[F]]

        val res = for {
          databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource

          _ <- Seq(
            blockHeaderSchema,
            blockBodySchema,
            ioTransactionSchema,
            canonicalHeadSchema,
            lockAddressSchema,
            txoSchema,
            groupPolicySchema,
            seriesPolicySchema
          )
            .traverse(OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, _))
            .void
            .toResource

          _ <- Seq(
            blockHeaderEdge,
            blockHeaderBodyEdge,
            blockHeaderTxIOEdge,
            blockHeaderRewardEdge,
            addressTxIOEdge,
            addressTxoEdge
          )
            .traverse(e => OrientDBMetadataFactory.createEdge[F](databaseDocumentTx, e))
            .void
            .toResource

          blockData = BlockData(
            blockHeader.copy(height = 1),
            blockBody
          )

          dbTx <- oThread.delay(odbFactory.getTx).toResource

          graphBlockUpdater <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)
          _                 <- graphBlockUpdater.insert(blockData).toResource

          blockHeaderVertex = dbTx.getBlockHeader(blockData.header)
          _ = assert(blockHeaderVertex.isDefined)
          _ <- assertIOBoolean(oThread.delay(dbTx.getBlockHeader(blockData.header).isDefined)).toResource
          _ <- assertIOBoolean(oThread.delay(dbTx.getBody(blockHeaderVertex.get).isDefined)).toResource

          _ <- graphBlockUpdater.remove(blockData).toResource

          // When we remove headerVertex, the canonicalHead schema is not updated, insertions handle it
          // When we remove txoVertex, lockAddress schema is not updated, because lockAddress references many txo
          // Eventually could create an orphan lockAddress, it is not a problem cause some future txo will reference it
          blockHeaderVertex = dbTx.getBlockHeader(blockData.header)
          _ = assert(blockHeaderVertex.isEmpty)
          _ <- assertIOBoolean(oThread.delay(dbTx.getBlockHeader(blockData.header).isEmpty)).toResource
          _ <- assertIOBoolean(oThread.delay(dbTx.getVerticesOfClass(blockBodySchema.name).asScala.isEmpty)).toResource
          _ <- assertIOBoolean(
            oThread.delay(dbTx.getVerticesOfClass(ioTransactionSchema.name).asScala.isEmpty)
          ).toResource
          _ <- assertIOBoolean(oThread.delay(dbTx.getVerticesOfClass(txoSchema.name).asScala.isEmpty)).toResource
          _ <- assertIOBoolean(
            oThread.delay(dbTx.getVerticesOfClass(groupPolicySchema.name).asScala.isEmpty)
          ).toResource
          _ <- assertIOBoolean(
            oThread.delay(dbTx.getVerticesOfClass(seriesPolicySchema.name).asScala.isEmpty)
          ).toResource

        } yield ()

        res.use_
      }
    }
  }
}
