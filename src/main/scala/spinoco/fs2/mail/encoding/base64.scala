package spinoco.fs2.mail.encoding

import fs2.Chunk.ByteVectorChunk
import fs2._
import scodec.bits.Bases.{Alphabets, Base64Alphabet}
import scodec.bits.{BitVector, ByteVector}


object base64 {

  /**
    * Encodes bytes to base64 encoded bytes [[http://tools.ietf.org/html/rfc4648#section-5 RF4648 section 5]]
    * Encoding is done lazily to support large Base64 bodies i.e. email, attachments..)
    * @param alphabet   Alphabet to use
    * @return
    */
  def encodeRaw[F[_]](alphabet:Base64Alphabet):Pipe[F, Byte, Byte] = {
    def go(rem:ByteVector)(s: Stream[F,Byte]):Pull[F, Byte, Unit] = {
      s.pull.uncons.flatMap {
        case None =>
          if (rem.size == 0) Pull.done
          else Pull.output(ByteVectorChunk(ByteVector.view(rem.toBase64(alphabet).getBytes)))

        case Some((chunk, h)) =>
          val bs = chunk.toBytes
          val n = rem ++ ByteVector.view(bs.values, bs.offset, bs.size)
          if (n.size/3 > 0) {
            val pad = n.size % 3
            val enc = n.dropRight(pad)
            val out = Array.ofDim[Byte]((enc.size.toInt / 3) * 4)
            var pos = 0
            enc.toBitVector.grouped(6) foreach { group =>
              val idx = group.padTo(8).shiftRight(2, signExtension = false).toByteVector.head
              out(pos) = alphabet.toChar(idx).toByte
              pos = pos + 1
            }
            Pull.output(ByteVectorChunk(ByteVector.view(out))) >> go(n.takeRight(pad))(h)
          } else {
            go(n)(h)
          }

      }

    }
    go(ByteVector.empty)(_).stream
  }

  /** encodes base64 encoded stream [[http://tools.ietf.org/html/rfc4648#section-5 RF4648 section 5]]. Whitespaces are ignored **/
  def encodeUrl[F[_]]: Pipe[F, Byte, Byte] =
    encodeRaw(Alphabets.Base64Url)

  /** encodes base64 encoded stream [[http://tools.ietf.org/html/rfc4648#section-4 RF4648 section 4]] **/
  def encode[F[_]]: Pipe[F, Byte, Byte] =
    encodeRaw[F](Alphabets.Base64)



  /**
    * Decodes base64 encoded stream with supplied alphabet. Whitespaces are ignored.
    * Decoding is lazy to support very large Base64 bodies (i.e. email)
    */
  def decodeRaw[F[_]: RaiseThrowable](alphabet:Base64Alphabet):Pipe[F, Byte, Byte] = {
    val Pad = alphabet.pad
    def go(remAcc:BitVector)(s:Stream[F, Byte]):Pull[F, Byte, Unit] = {
      s.pull.uncons.flatMap {
        case None => Pull.done

        case Some((chunk,tl)) =>
          val bs = chunk.toBytes
          val bv = ByteVector.view(bs.values, bs.offset, bs.size)
          var acc = remAcc
          var idx = 0
          var term = false
          try {
            bv.foreach  { b =>
              b.toChar match {
                case c if alphabet.ignore(c) => // ignore no-op
                case Pad => term = true
                case c =>
                  if (!term) acc = acc ++ BitVector(alphabet.toIndex(c)).drop(2)
                  else {
                    throw new IllegalArgumentException(s"Unexpected character '$c' at index $idx after padding character; only '=' and whitespace characters allowed after first padding character")
                  }
              }
              idx = idx + 1
            }
            val aligned = (acc.size / 8) * 8
            if (aligned <= 0 && !term) go(acc)(tl)
            else {
              val (out, rem) = acc.splitAt(aligned)
              if (term) Pull.output(ByteVectorChunk(out.toByteVector))
              else Pull.output(ByteVectorChunk(out.toByteVector)) >> go(rem)(tl)
            }

          } catch {
            case e: IllegalArgumentException =>
              Pull.raiseError(new Throwable(s"Invalid base 64 encoding at index $idx", e))
          }
      }
    }
    go(BitVector.empty)(_).stream
  }

  /** decodes base64 encoded stream [[http://tools.ietf.org/html/rfc4648#section-5 RF4648 section 5]]. Whitespaces are ignored **/
  def decodeUrl[F[_]: RaiseThrowable]:Pipe[F, Byte, Byte] =
    decodeRaw(Alphabets.Base64Url)

  /** decodes base64 encoded stream [[http://tools.ietf.org/html/rfc4648#section-4 RF4648 section 4]] **/
  def decode[F[_]: RaiseThrowable]:Pipe[F, Byte, Byte] =
    decodeRaw(Alphabets.Base64)

}
