package co.topl.genusLibrary.interpreter

import cats.effect._
import cats.implicits._
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.algebras.BlockInserterAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genusLibrary.orientDb.instances.SchemaBlockHeader.Field
import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances._
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import org.typelevel.log4cats.Logger

import scala.util.Try

object GraphBlockInserter {

  def make[F[_]: OrientThread: Logger](graph: OrientGraph): Resource[F, BlockInserterAlgebra[F]] =
    Resource.pure(
      new BlockInserterAlgebra[F] {

        override def insert(block: BlockData): F[Either[GE, Unit]] =
          OrientThread[F].delay {
            Try {
              val headerVertex = graph.addHeader(block.header)

              graph.addCanonicalHead(headerVertex)

              val bodyVertex = graph.addBody(block.body)
              bodyVertex.setProperty(blockBodySchema.links.head.propertyName, headerVertex.getId)

              // Relationships between Header <-> TxIOs
              block.transactions.map { ioTx =>
                val txVertex = graph.addIoTx(ioTx)
                txVertex.setProperty(ioTransactionSchema.links.head.propertyName, headerVertex.getId)
                graph.addEdge(s"class:${blockHeaderTxIOEdge.name}", headerVertex, txVertex, blockHeaderTxIOEdge.label)
              }

              // Relationship between Header <-> ParentHeader if Not Genesis block
              if (block.header.height != 1) {
                val headerInVertex = graph
                  .getVertices(Field.BlockId, block.header.parentHeaderId.value.toByteArray)
                  .iterator()
                  .next()

                graph.addEdge(s"class:${blockHeaderEdge.name}", headerVertex, headerInVertex, blockHeaderEdge.label)
              }
              graph.commit()
            }.toEither
              .leftMap { th =>
                graph.rollback()
                GEs.InternalMessage(th.getMessage): GE
              }
          }

      }
    )

}
