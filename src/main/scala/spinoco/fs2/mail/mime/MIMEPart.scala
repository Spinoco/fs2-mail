package spinoco.fs2.mail.mime

import java.nio.charset.{Charset, StandardCharsets}
import java.util.UUID

import spinoco.protocol.mail.mime.{MIMEHeader, TransferEncoding}
import fs2._
import fs2.util.Effect
import spinoco.fs2.mail.encoding
import spinoco.fs2.mail.encoding.lines
import spinoco.protocol.mail.mime.TransferEncoding.{DefaultEncodings, StandardEncoding}
import spinoco.protocol.mime.{ContentDisposition, ContentType, MIMECharset, MediaType}
import spinoco.protocol.mime.MediaType.{DefaultMediaType, MultipartMediaType}



/** represnet single or multi MIME part **/
sealed trait MIMEPart[F[_]] {
  def header: MIMEHeader
}


object MIMEPart {

  /**
    * Represents a mime part within the Email body
    * @param header     Header of the Mime part
    * @param data       Data expressed as bytes, encoded according to information provided in header.
    *                   For multipart part, this is empty.
    * @tparam F
    */
  case class SinglePart[F[_]](
    header: MIMEHeader
    , data: Stream[F, Byte]
  ) extends MIMEPart[F]

  /**
    * A mime part that is multipart. i.e. instead containing data directly, this will contain other mime
    * parts
    * @param header   Header of the multipart
    * @param boundary Boundary identification, to divide parts accoding to RFC
    * @param parts    Parts to eb included.
    * @tparam F
    */
  case class MultiPart[F[_]](
    header: MIMEHeader
    , boundary: String
    , parts: Stream[F, MIMEPart[F]]
  ) extends MIMEPart[F]


  /**
    * Creates a MIME part, that encodes supplied stream of chars as `encoding` with type of text/html and supplied charset.
    *
    * @param stream       Stream of characters
    * @param charset      Charset to encode the characters to
    * @param enc          Encoding to use
    */
  def html[F[_] : Effect](
    stream: Stream[F, Char]
    , charset: Charset = StandardCharsets.UTF_8
    , enc: StandardEncoding = TransferEncoding.QuotedPrintable
  ): SinglePart[F] =
    text(stream, MediaType.`text/html`, charset, enc)

  /**
    * Creates a MIME part, that encodes supplied stream of chars as `encoding` with type of text/plain and supplied charset.
    *
    * @param stream       Stream of characters
    * @param charset      Charset to encode the characters to
    * @param enc          Encoding to use
    */
  def plain[F[_] : Effect](
    stream: Stream[F, Char]
    , charset: Charset = StandardCharsets.UTF_8
    , enc: StandardEncoding = TransferEncoding.QuotedPrintable
  ): SinglePart[F] =
    text(stream, MediaType.`text/plain`, charset, enc)

  /**
    * Creates a MIME part, that encodes supplied stream of chars as `encoding` with type of `tpe` and supplied charset.
    *
    * @param stream       Stream of characters
    * @param tpe          Type of the content
    * @param charset      Charset to encode the characters to
    * @param enc          Encoding to use
    */
  def text[F[_] : Effect](
    stream: Stream[F, Char]
    , tpe: DefaultMediaType
    , charset: Charset = StandardCharsets.UTF_8
    , enc: StandardEncoding = TransferEncoding.QuotedPrintable
  ): SinglePart[F] = {
    val data =
      stream through encoding.charset.encode(charset) through textEncoder(enc)
    SinglePart(
      header =
        MIMEHeader(List.empty)
        .contentType(ContentType.TextContent(tpe, Some(MIMECharset.forJavaCharset(charset))))
        .transferEncoding(enc)
      , data = data
    )
  }

  /** for supplied encoding creates an encoder **/
  def textEncoder[F[_]](enc: StandardEncoding): Pipe[F, Byte, Byte] = {
    enc.encoding match {
      case DefaultEncodings.QuotedPrintable => encoding.quotedPrintable.encode[F]
      case DefaultEncodings.Base64 => encoding.base64.encode[F] andThen lines.blockLines()
      case DefaultEncodings.Bits7 => identity[Stream[F, Byte]]
      case DefaultEncodings.Bits8 => identity[Stream[F, Byte]]
      case DefaultEncodings.Binary => identity[Stream[F, Byte]]
    }
  }

  /**
    * Creates a MIME part, that encodes binary stream with supplied encdoing any media type.
    * @param stream     Stream of byted
    * @param tpe        Type of the media of the binary content
    * @param id         Id fo the content
    */
  def binary[F[_]](
    id: String
    , tpe: DefaultMediaType
    , stream: Stream[F, Byte]
  ): SinglePart[F] = {
    SinglePart(
      header =
        MIMEHeader(List.empty)
        .contentType(ContentType.BinaryContent(tpe, None))
        .transferEncoding(TransferEncoding.Base64)
        .contentId(id)
      , data = stream through encoding.base64.encode through lines.blockLines()
    )
  }

  /**
    * Allows to mark the attachement as the file
    * @param id           Id of the content
    * @param fileName
    * @param tpe
    * @param stream
    * @tparam F
    * @return
    */
  def file[F[_]](
    id: String
    , fileName: String
    , tpe: DefaultMediaType
    , stream: Stream[F, Byte]
  ): SinglePart[F] = {
    val bin = binary(id, tpe, stream)
    bin.copy(header = bin.header.contentDisposition(ContentDisposition("attachment", Map("filename" -> fileName))))
  }


  /**
    * Creates a MIME part with alternative message content.
    *
    * @param fallback       A content that is used as fallback (i.e. text/plain) if none of the alternatives work.
    * @param alternative    An alternative content to send (i.e. text/html)
    * @param boundary       A boundary string denoting one part. This string cannot be longer than 70 characters and must not
    *                       contain any whitespaces.
    */
  @deprecated("Deprecated in favour of [[MIMEPart.alternative]]", "0.1.1")
  def alternate[F[_]](
    fallback: MIMEPart[F]
    , alternative: MIMEPart[F]
    , boundary: => String = s"---${ UUID.randomUUID() }---${ UUID.randomUUID() }---"
  ): MultiPart[F] =
    MIMEPart.alternative(fallback, alternative, boundary)

  /**
    * Creates a MIME part with alternative message content.
    *
    * @param fallback       A content that is used as fallback (i.e. text/plain) if none of the alternatives work.
    * @param alternative    An alternative content to send (i.e. text/html)
    * @param boundary       A boundary string denoting one part. This string cannot be longer than 70 characters and must not
    *                       contain any whitespaces.
    */
  def alternative[F[_]](
    fallback: MIMEPart[F]
    , alternative: MIMEPart[F]
    , boundary: => String = s"---${ UUID.randomUUID() }---${ UUID.randomUUID() }---"
  ): MultiPart[F] =
    multipart(subtype = "alternative", parts = Stream(fallback, alternative), boundary = boundary)

  /**
    * Creates a MIME part with mixed message content.
    *
    * The separate parts will be displayed in sequence. The inner parts will be displayed properly, meaning
    * if there is an alternative in the mixed parts, the alternative will work as if it were on top level.
    *
    * @param parts    The parts to make this mixed type
    * @param boundary A boundary string denoting one part. This string cannot be longer than 70 characters and must not
    *                 contain any whitespaces.
    */
  def mixed[F[_]](
    parts: Stream[F, MIMEPart[F]]
    , boundary: => String = s"---${ UUID.randomUUID() }---${ UUID.randomUUID() }---"
  ): MultiPart[F] =
    multipart(subtype = "mixed", parts = parts, boundary = boundary)

  /**
    * Creates a MIME part with related message content.
    * @see https://tools.ietf.org/html/rfc2387
    *
    * @param mainPart       Displayed part. Contains links to related parts (cid:Content-Id)
    * @param related        Related parts referenced from mainPart.
    * @param boundary       A boundary string denoting one part. This string cannot be longer than 70 characters and must not
    *                       contain any whitespaces.
    */
  def related[F[_]](
    mainPart: SinglePart[F]
    , related: Stream[F, MIMEPart[F]]
    , boundary: => String = s"---${ UUID.randomUUID() }---${ UUID.randomUUID() }---"
  ): MultiPart[F] = {
    multipart(subtype = "related", parts = Stream(mainPart) ++ related, boundary = boundary)
  }

  /**
    * Creates a multipart with supplied subtype and encoding.
    *
    * @param subtype    Subtype (i.e. alternate/mixed)
    * @param encoding   Encoding (note the 7Bit, 8Bit, Binary must be used according to RFC
    * @param parts      Parts to include in this mime part. Parts may be nested.
    * @param boundary   A boundary string denoting one part. This string cannot be longer than 70 characters and must not
    *                   contain any whitespaces.
    */
  def multipart[F[_]](
    subtype: String
    , parts: Stream[F, MIMEPart[F]]
    , encoding: TransferEncoding = TransferEncoding.Bits8
    , boundary: => String = s"---${ UUID.randomUUID() }---${ UUID.randomUUID() }---"
  ): MultiPart[F] = {
    val boundaryId = boundary
    val mediaType = MultipartMediaType(subtype, Map("boundary" -> boundaryId))
    MultiPart(
      header =
        MIMEHeader(List.empty)
        .contentType(ContentType.MultiPartContent(mediaType, None))
        .transferEncoding(encoding)
      , boundary = boundaryId
      , parts = parts
    )
  }

}