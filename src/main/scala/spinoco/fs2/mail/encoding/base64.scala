package spinoco.fs2.mail.encoding

import java.nio.{Buffer, CharBuffer}

import cats.effect.Sync
import fs2._
import scodec.bits.Bases.{Alphabets, Base64Alphabet}
import scodec.bits.{Bases, ByteVector}
import spinoco.fs2.mail.interop
import spinoco.fs2.mail.interop.StringChunk


object base64 {

  def encodeRaw[F[_]](alphabet: Base64Alphabet):Pipe[F, Byte, Byte] = { source =>
    source
    .through(base64Encode[F](alphabet))
    .flatMap(s => Stream.chunk(Chunk.bytes(s.getBytes)))
  }

  /** encodes base64 encoded stream [[http://tools.ietf.org/html/rfc4648#section-5 RF4648 section 5]]. Whitespaces are ignored **/
  def encodeUrl[F[_]]: Pipe[F, Byte, Byte] =
    encodeRaw(Alphabets.Base64Url)

  /** encodes base64 encoded stream [[http://tools.ietf.org/html/rfc4648#section-4 RF4648 section 4]] **/
  def encode[F[_]]: Pipe[F, Byte, Byte] =
    encodeRaw[F](Alphabets.Base64)

  def decodeRaw[F[_]: Sync](alphabet:Base64Alphabet):Pipe[F, Byte, Byte]  = { source =>
    source
    .through(charset.decodeUTF8[F])
    .chunks
    .map(StringChunk.asString)
    .through(base64Decode(alphabet))
  }

  def decodeUrl[F[_]: Sync]:Pipe[F, Byte, Byte] =
    decodeRaw(Alphabets.Base64Url)

  def decode[F[_]: Sync]:Pipe[F, Byte, Byte] =
    decodeRaw(Alphabets.Base64)

  // Taken from fs2 upstream, until we migrate to newest fs2 we need correct encode / decode

  /**
    * Converts a stream of base 64 text in to a stream of bytes.
    *
    * If the text is not valid base 64, the pipe fails with an exception. Padding
    * characters at the end of the input stream are optional, but if present, must
    * be valid per the base 64 specification. Whitespace characters are ignored.
    *
    * The default base 64 alphabet is used by this pipe.
    */
  def base64Decode[F[_]]: Pipe[F, String, Byte] =
    base64Decode(Bases.Alphabets.Base64)

  /**
    * Like [[base64Decode]] but takes a base 64 alphabet. For example,
    * `base64Decode(Bases.Alphabets.Base64Url)` will decode URL compatible base 64.
    */
  def base64Decode[F[_]](alphabet: Bases.Base64Alphabet): Pipe[F, String, Byte] = {
    // Adapted from scodec-bits, licensed under 3-clause BSD
    final case class State(buffer: Int, mod: Int, padding: Int)
    val Pad = alphabet.pad
    def paddingError =
      Left(
        "Malformed padding - final quantum may optionally be padded with one or two padding characters such that the quantum is completed"
      )

    def decode(state: State, str: String): Either[String, (State, Chunk[Byte])] = {
      var buffer = state.buffer
      var mod = state.mod
      var padding = state.padding
      var idx, bidx = 0
      val acc = new Array[Byte]((str.size + 3) / 4 * 3)
      while (idx < str.length) {
        str(idx) match {
          case c if alphabet.ignore(c) => // ignore
          case c =>
            val cidx = {
              if (padding == 0) {
                if (c == Pad) {
                  if (mod == 2 || mod == 3) {
                    padding += 1
                    0
                  } else {
                    return paddingError
                  }
                } else {
                  try alphabet.toIndex(c)
                  catch {
                    case _: IllegalArgumentException =>
                      return Left(s"Invalid base 64 character '$c' at index $idx")
                  }
                }
              } else {
                if (c == Pad) {
                  if (padding == 1 && mod == 3) {
                    padding += 1
                    0
                  } else {
                    return paddingError
                  }
                } else {
                  return Left(
                    s"Unexpected character '$c' at index $idx after padding character; only '=' and whitespace characters allowed after first padding character"
                  )
                }
              }
            }
            mod match {
              case 0 =>
                buffer = (cidx & 0x3f)
                mod += 1
              case 1 | 2 =>
                buffer = (buffer << 6) | (cidx & 0x3f)
                mod += 1
              case 3 =>
                buffer = (buffer << 6) | (cidx & 0x3f)
                mod = 0
                acc(bidx) = (buffer >> 16).toByte
                acc(bidx + 1) = (buffer >> 8).toByte
                acc(bidx + 2) = buffer.toByte
                bidx += 3
            }
        }
        idx += 1
      }

      val paddingInBuffer = if (mod == 0) padding else 0
      val out = Chunk.byteVector(ByteVector.view(acc).take((bidx - paddingInBuffer).toLong))
      val carry = State(buffer, mod, padding)
      Right((carry, out))
    }

    def finish(state: State): Either[String, Chunk[Byte]] = {
      if (state.padding != 0 && state.mod != 0) paddingError
      else
        state.mod match {
          case 0 => Right(Chunk.empty)
          case 1 => Left("Final base 64 quantum had only 1 digit - must have at least 2 digits")
          case 2 =>

            Right(Chunk((state.buffer >> 4).toByte))

          case 3 =>
            val buffer = state.buffer
            Right(
              Chunk(
                (buffer >> 10).toByte,
                (buffer >> 2).toByte
              )
            )
        }
    }

    def go(state: State, s: Stream[F, String]): Pull[F, Byte, Unit] =
      s.pull.uncons1.flatMap {
        case Some((hd, tl)) =>
          decode(state, hd) match {
            case Right((newState, out)) =>
              Pull.output(out) >> go(newState, tl)
            case Left(err) => Pull.raiseError(new IllegalArgumentException(err))
          }
        case None =>
          finish(state) match {
            case Right(out) => Pull.output(out)
            case Left(err)  => Pull.raiseError(new IllegalArgumentException(err))
          }
      }

    in => go(State(0, 0, 0), in).stream
  }

  /**
    * Encodes a byte stream in to a stream of base 64 text.
    * The default base 64 alphabet is used by this pipe.
    */
  def base64Encode[F[_]]: Pipe[F, Byte, String] = base64Encode(Bases.Alphabets.Base64)

  /**
    * Like [[base64Encode]] but takes a base 64 alphabet. For example,
    * `base64Encode(Bases.Alphabets.Base64Url)` will encode URL compatible base 64.
    */
  def base64Encode[F[_]](alphabet: Bases.Base64Alphabet): Pipe[F, Byte, String] = {
    // Adapted from scodec-bits, licensed under 3-clause BSD
    def encode(c: ByteVector): (String, ByteVector) = {
      val bytes = c.toArray
      val bldr = CharBuffer.allocate(((bytes.length + 2) / 3) * 4)
      var idx = 0
      val mod = bytes.length % 3
      while (idx < bytes.length - mod) {
        var buffer = ((bytes(idx) & 0xff) << 16) | ((bytes(idx + 1) & 0xff) << 8) | (bytes(
          idx + 2
        ) & 0xff)
        val fourth = buffer & 0x3f
        buffer = buffer >> 6
        val third = buffer & 0x3f
        buffer = buffer >> 6
        val second = buffer & 0x3f
        buffer = buffer >> 6
        val first = buffer
        bldr
          .append(alphabet.toChar(first))
          .append(alphabet.toChar(second))
          .append(alphabet.toChar(third))
          .append(alphabet.toChar(fourth))
        idx = idx + 3
      }
      (bldr: Buffer).flip
      val out = bldr.toString
      if (mod == 0) {
        (out, ByteVector.empty)
      } else if (mod == 1) {
        (out, ByteVector(bytes(idx)))
      } else {
        (out, ByteVector(bytes(idx), bytes(idx + 1)))
      }
    }

    def go(carry: ByteVector, s: Stream[F, Byte]): Pull[F, String, Unit] =
      s.pull.uncons.flatMap {
        case Some((hd, tl)) =>
          val (out, newCarry) = encode(carry ++ interop.toByteVector(hd))
          Pull.output1(out) >> go(newCarry, tl)
        case None =>
          carry.size match {
            case 0 => Pull.done
            case 1 =>
              var buffer = (carry(0) & 0xff) << 4
              val second = buffer & 0x3f
              buffer = buffer >> 6
              val first = buffer
              val out = new String(
                Array(alphabet.toChar(first), alphabet.toChar(second), alphabet.pad, alphabet.pad)
              )
              Pull.output1(out)
            case 2 =>
              var buffer = ((carry(0) & 0xff) << 10) | ((carry(1) & 0xff) << 2)
              val third = buffer & 0x3f
              buffer = buffer >> 6
              val second = buffer & 0x3f
              buffer = buffer >> 6
              val first = buffer
              val out = new String(
                Array(
                  alphabet.toChar(first),
                  alphabet.toChar(second),
                  alphabet.toChar(third),
                  alphabet.pad
                )
              )
              Pull.output1(out)
            case other => sys.error(s"carry must be size 0, 1, or 2 but was $other")
          }
      }

    in => go(ByteVector.empty, in).stream
  }

}
