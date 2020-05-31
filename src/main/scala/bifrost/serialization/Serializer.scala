package bifrost.serialization

import java.nio.ByteBuffer

import akka.util.ByteString
import bifrost.utils.ByteArrayBuilder
import bifrost.utils.serialization._

import scala.util.Try

trait Serializer[TFamily, T <: TFamily, R <: Reader, W <: Writer] {

  def serialize(obj: T, w: W): Unit

  def parse(r: R): TFamily

  def parseTry(r: R): Try[TFamily] = {
    Try(parse(r))
  }

  def toByteString(obj: T): ByteString = {
    val writer = new VLQByteStringWriter()
    serialize(obj, writer)
    writer.result()
  }

  def parseByteString(byteString: ByteString): T = {
    val reader = new VLQByteStringReader(byteString)
    parse(reader)
  }

  def parseByteStringTry(byteString: ByteString): Try[T] = {
    Try(parseByteString(byteString))
  }

  def toBytes(obj: T): Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    serialize(obj, writer)
    writer.result().toBytes
  }

  def parseBytes(bytes: Array[Byte]): T = {
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    parse(reader)
  }

  def parseBytesTry(bytes: Array[Byte]): Try[T] = {
    Try(parseBytes(bytes))
  }
}