package spinoco.fs2.mail

import fs2.Chunk
import scodec.bits.ByteVector

package object interop {

  // Converts byte chunk into byte vector, delete this once on new fs2 as this is on chunk
  def toByteVector(chunk: Chunk[Byte]): ByteVector = {
    chunk match {
      case c: Chunk.ByteVectorChunk => c.toByteVector
      case other                    => ByteVector.view(other.toArray)
    }
  }

}
