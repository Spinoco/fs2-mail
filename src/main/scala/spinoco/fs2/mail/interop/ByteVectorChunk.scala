package spinoco.fs2.mail.interop

import fs2.Chunk
import scodec.bits.ByteVector

import scala.reflect.{ClassTag, classTag}

// Will be replaced in 0.10 by fs2-scodec interop
final class ByteVectorChunk private (val toByteVector: ByteVector)
  extends Chunk[Byte] {
  def apply(i: Int): Byte =
    toByteVector(i)

  def copyToArray[B >: Byte](xs: Array[B], start: Int): Unit =
    xs match {
      case byteArray: Array[Byte] =>
        toByteVector.copyToArray(byteArray, start)
      case _ =>
        iterator.copyToArray(xs, start)
    }

  def drop(n: Int): Chunk[Byte] =
    ByteVectorChunk(toByteVector.drop(n))

  def filter(f: Byte => Boolean): Chunk[Byte] = {
    var i = 0
    val bound = toByteVector.size

    val values2 = new Array[Byte](size)
    var size2 = 0

    while (i < bound) {
      val b = toByteVector(i)
      if (f(b)) {
        values2(size2) = toByteVector(i)
        size2 += 1
      }

      i += 1
    }

    ByteVectorChunk(ByteVector.view(values2).take(size2))
  }

  def foldLeft[B](z: B)(f: (B, Byte) => B): B =
    toByteVector.foldLeft(z)(f)

  def foldRight[B](z: B)(f: (Byte, B) => B): B =
    toByteVector.foldRight(z)(f)

  def size: Int =
    toByteVector.size.toInt

  def take(n: Int): Chunk[Byte] =
    ByteVectorChunk(toByteVector.take(n))

  protected val tag: ClassTag[_] =
    classTag[Byte]


}

object ByteVectorChunk {
  def apply(bv: ByteVector): ByteVectorChunk =
    new ByteVectorChunk(bv)

  def asByteVector(ch: Chunk[Byte]): ByteVector = {
    ch match {
      case bvc: ByteVectorChunk => bvc.toByteVector
      case other =>
        val bs = other.toBytes
        ByteVector.view(bs.values, bs.offset, bs.size)

    }
  }

}