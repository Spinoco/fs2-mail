package spinoco.fs2.mail.encoding

import fs2._
import fs2.interop.scodec.ByteVectorChunk
import scodec.bits.Bases.Alphabets.HexUppercase
import scodec.bits.ByteVector

import scala.annotation.tailrec

object quotedPrintable {

  private val `=` = ByteVector.view("=".getBytes)
  private val `\r` = ByteVector.view("\r".getBytes)
  private val `\n` = ByteVector.view("\n".getBytes)
  private val crlf = ByteVector.view("\r\n".getBytes)
  private val `=crlf` = ByteVector.view("=\r\n".getBytes)
  private val MAX_LINE_SZ = 75

  /**
    * Decodes bytes to string chunks based on quoted printable stream.
    * The string output represents the input lines in 7bit encoding.
    * @tparam F
    * @return
    */
  def decode[F[_]]: Pipe[F, Byte, Byte] = {
     @tailrec
    def decodeBV(rem: ByteVector, acc: ByteVector):Either[String, (ByteVector, ByteVector)] = {
      val eqIdx = rem.indexOfSlice(`=`)
      if (eqIdx < 0) Right((acc ++ rem, ByteVector.empty))
      else {
        val (ok, next) = rem.splitAt(eqIdx)
        if (next.size < 3) Right((acc ++ ok, next))
        else {
          val code = next.tail.take(2)
          if (code == crlf) decodeBV(next.drop(3), acc ++ ok)
          else {
            ByteVector.fromHexDescriptive(new String(next.tail.take(2).toArray), HexUppercase) match {
              case Right(bv) =>
                decodeBV(next.drop(3), acc ++ ok ++ bv)
              case Left(_) =>
                decodeBV(next.tail, acc ++ ok)
            }
          }
        }
      }
    }


    def go(buff: ByteVector)(s: Stream[F, Byte]): Pull[F, Byte, Unit] = {
      s.pull.unconsChunk.flatMap {
        case Some((chunk, tl)) =>
          val bs = chunk.toBytes
          val bv = buff ++ ByteVector.view(bs.values, bs.offset, bs.size)
          decodeBV(bv, ByteVector.empty) match {
            case Right((decoded, remainder)) =>
              Pull.output(ByteVectorChunk(decoded)) >> go(remainder)(tl)

            case Left(err) =>
              Pull.raiseError(new Throwable(s"Failed to decode from quotedPrintable: $err (${bv.decodeUtf8})"))
          }

        case None =>
          if (buff.isEmpty || buff == `=`) Pull.done
          else Pull.raiseError(new Throwable(s"Unfinished bytes from quoted-printable: $buff"))
      }
    }


    go(ByteVector.empty)(_).stream
  }




  /**
   * Encodes the supplied characters as Quoted printable
   * Expects that every line separated by hard-line break (crlf) to be delivered as exactly one chunk
   *
   * Outputs as stream of quoted-printable bytes. With soft-break inserted as necessary.
   *
   *
   */
  def encode[F[_]]: Pipe[F, Byte, Byte] = {

    def isPrintable(b: Byte): Boolean = {
      (b >= 33 && b <= 126 && b != '=')  ||  // any printable, except `=`
        (b == 9 || b == 32)  // tab or space
    }

    // encodes one line spearated by crlf (hard break)
    def encodeLine(bv: ByteVector): ByteVector = {
      @tailrec
      def go(rem: ByteVector, acc:ByteVector, untilBreak: Long): ByteVector = {
        if (rem.isEmpty) acc
        else if (!isPrintable(rem.head)) {
          if (untilBreak < 3) {
            // we couldn't fully encode this character and softwrap
            // reset to full line, add softwrap and continue
            go(rem, acc ++ `=crlf`, MAX_LINE_SZ)
          } else {
            val encoded = `=` ++ ByteVector.view(rem.take(1).toHex(HexUppercase).getBytes)
            go (rem.tail, acc ++ encoded, untilBreak - encoded.size)
          }
        } else {
          val nextLine = rem takeWhile(isPrintable) take(untilBreak)
          if (nextLine.size == untilBreak && (rem.size > untilBreak)) {
            // this indicates we have yet bytes coming after this line
            go(rem.drop(nextLine.size), acc ++ nextLine ++ `=crlf`, MAX_LINE_SZ)
          } else if (nextLine.size == untilBreak) {
            go(rem.drop(nextLine.size), acc ++ nextLine, MAX_LINE_SZ)
          } else {
            go(rem.drop(nextLine.size), acc ++ nextLine, untilBreak - nextLine.size)
          }
        }
      }
      go(bv, ByteVector.empty, MAX_LINE_SZ)
    }

    _.through(lines.byCrLf)
    .map { ch =>
      val bs = ch.toBytes
      ByteVector.view(bs.values, bs.offset, bs.size)
    }
    .map(encodeLine)
    .intersperse(crlf)
    .flatMap { bv => Stream.chunk(ByteVectorChunk(bv)) }

  }



}
