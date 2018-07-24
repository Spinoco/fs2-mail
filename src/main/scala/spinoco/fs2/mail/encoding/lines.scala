package spinoco.fs2.mail.encoding

import fs2._
import fs2.interop.scodec.ByteVectorChunk
import scodec.bits.ByteVector

import scala.annotation.tailrec


object lines {

  /** decodes bytes in chunk of bytes by supplied separator. Last line is emitted even when not terminated by `separator` **/
  def by[F[_]](separator: ByteVector): Pipe[F, Byte, Chunk[Byte]] = {
    def go(buff: ByteVector)(s: Stream[F, Byte]): Pull[F, Chunk[Byte], Unit] = {
      s.pull.unconsChunk.flatMap {
        case Some((ch, tl)) =>
          val bs = ch.toBytes
          val data = buff ++ ByteVector.view(bs.values, bs.offset, bs.size)
          val idx = data.indexOfSlice(separator)
          if (idx < 0) go(data)(tl)
          else {
            val (h, t0) = data.splitAt(idx)
            if (t0.isEmpty) Pull.output1(ByteVectorChunk(h)) >> go(ByteVector.empty)(tl)
            else Pull.output1(ByteVectorChunk(h)) >> go(ByteVector.empty)(Stream.chunk(ByteVectorChunk(t0.drop(separator.size))) ++ tl)
          }

        case None =>
          Pull.output1(ByteVectorChunk(buff))
      }
    }
    go(ByteVector.empty)(_).stream
  }

  /** decodes bytes to chunks according to supplied separator **/
  def byCrLf[F[_]]: Pipe[F, Byte, Chunk[Byte]] =
    by(ByteVector.view("\r\n".getBytes))

  private val crlf = ByteVector.view("\r\n".getBytes)

  /**
    * Splits bytes by lines so the first character is prepended by given `prefix` number of spaces and
    * up to `length` of  characters are included on every line.
    *
    * Note that if there are any crlf characters , then the line will be terminated as expected.
    *
    * @param prefix   Prefix size of spaces
    * @param length   max number of bytes to include onsingle line, excluding prefix.
    * @tparam F
    * @return
    */
  def blockLines[F[_]](prefix: Int = 2, length: Int = 73): Pipe[F, Byte, Byte] = {
    val prefixBytes = ByteVector.view((" " * prefix).getBytes)
    def go(buff: ByteVector)(s: Stream[F, Byte]): Pull[F, Byte, Unit] = {

      @tailrec
      def makeLines(bv: ByteVector, result: ByteVector = ByteVector.empty)(tl: Stream[F, Byte]): Pull[F, Byte, Unit] = {
        val idx = bv.indexOfSlice(crlf)
        if (idx < 0) {
          val head = bv.take(length)
          if (head.size < length) {
            if (result.nonEmpty) {
              Pull.outputChunk(ByteVectorChunk(result)) >> go(bv)(tl)
            } else {
              go(bv)(tl)
            }
          } else {
            val chunks = bv.grouped(length)
            val lastChunk = chunks.lastOption.getOrElse(ByteVector.empty)
            val chunksOut = if (lastChunk.nonEmpty) chunks.init else chunks
            Pull.outputChunk(ByteVectorChunk(result ++ ByteVector.concat(chunksOut.map(h => prefixBytes ++ h ++ crlf)))) >> go(lastChunk)(tl)
          }
        } else {
          val (head, t) = bv.splitAt(idx)
          val linesOut = head.grouped(length).map(h => prefixBytes ++ h ++ crlf)
          makeLines(t.drop(crlf.size), result ++ ByteVector.concat(linesOut))(tl)
        }
      }

      s.pull.unconsChunk.flatMap {
        case Some((ch, tl)) =>
          val bs = ch.toBytes
          val bv = buff ++ ByteVector.view(bs.values, bs.offset, bs.size)
          makeLines(bv)(tl)

        case None =>
          if (buff.isEmpty) Pull.done
          else Pull.outputChunk(ByteVectorChunk(prefixBytes ++ buff ++ crlf))
      }
    }
    go(ByteVector.empty)(_).stream
  }

}
