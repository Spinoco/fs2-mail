package spinoco.fs2.mail.encoding

import fs2.Chunk
import scodec.bits.ByteVector

object accumulate {

  def byteVector(acc: ByteVector, chunk: Chunk[Byte]): ByteVector = {
    acc ++ spinoco.fs2.mail.interop.toByteVector(chunk)
  }

}
