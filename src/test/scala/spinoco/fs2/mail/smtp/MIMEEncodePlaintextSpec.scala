package spinoco.fs2.mail.smtp

import java.nio.charset.Charset
import java.time.{ZoneId, ZonedDateTime}

import fs2._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import spinoco.fs2.mail.interop.{ByteVectorChunk, StringChunk}
import spinoco.protocol.mail.{EmailAddress, EmailHeader}
import spinoco.protocol.mail.header.codec.EmailHeaderCodec
import spinoco.protocol.mail.mime.TransferEncoding
import spinoco.protocol.mime.{ContentType, MIMECharset, MediaType}

object MIMEEncodePlaintextSpec extends Properties("MIMEEncodePlaintext") {

  val emailCodec = EmailHeaderCodec.codec(100 * 1024)
  val mimeCodec = EmailHeaderCodec.mimeCodec(10 * 1024)

  val input1 = Stream.chunk(StringChunk(
    """This is a short line
      |This ia a very long line that shall be split by fws to multiple line to make sure that SMTP server won't complain on size of it
      |This line of text contains accented characters like ěščřžýá that shall be encoded
      |. Dot starting line
      |--- end
      |
      """.stripMargin.lines.mkString("\r\n")
  )).covary[Task]

  property("defaults") = protect {

    SMTPClient.impl.encodeTextBody[Task](
      header = EmailHeader(
        subject = "Test Email"
        , date = ZonedDateTime.of(2017, 12, 12, 7, 32, 10, 0, ZoneId.of("GMT"))
        , from = EmailAddress("john.doe", "mail.com", Some("John Doe"))
        , to = EmailAddress("phil.doe", "mail.com", Some("Phil Doe"))
      )
      , text = input1
      , emailHeaderCodec = emailCodec
      , mimeHeaderCodec = mimeCodec
    )
    .chunks.map(ByteVectorChunk.asByteVector)
    .runLog.map { _.reduce(_ ++ _).decodeUtf8.right.getOrElse("--ERR--") }
    .unsafeRun() ?=
      """Subject: Test Email
        |Date: Tue, 12 Dec 2017 07:32:10 +0000
        |From: "John Doe" <john.doe@mail.com>
        |To: "Phil Doe" <phil.doe@mail.com>
        |Content-Type: text/plain; charset=utf-8
        |Content-Transfer-Encoding: quoted-printable
        |
        |This is a short line
        |This ia a very long line that shall be split by fws to multiple line to mak=
        |e sure that SMTP server won't complain on size of it
        |This line of text contains accented characters like =C4=9B=C5=A1=C4=8D=C5=
        |=99=C5=BE=C3=BD=C3=A1 that shall be encoded
        |. Dot starting line
        |--- end
        |
      """.stripMargin.lines.mkString("\r\n")

  }


  property("specified.content-type") = protect {

    SMTPClient.impl.encodeTextBody[Task](
      header = EmailHeader(
        subject = "Test Email"
        , date = ZonedDateTime.of(2017, 12, 12, 7, 32, 10, 0, ZoneId.of("GMT"))
        , from = EmailAddress("john.doe", "mail.com", Some("John Doe"))
        , to = EmailAddress("phil.doe", "mail.com", Some("Phil Doe"))
      ).contentType(ContentType.TextContent(MediaType.`text/plain`, Some(MIMECharset.forJavaCharset(Charset.forName("ISO-8859-2")))))
      , text = input1
      , emailHeaderCodec = emailCodec
      , mimeHeaderCodec = mimeCodec
    )
      .chunks.map(ByteVectorChunk.asByteVector)
      .runLog.map { _.reduce(_ ++ _).decodeUtf8.right.getOrElse("--ERR--") }
      .unsafeRun() ?=
      """Subject: Test Email
        |Date: Tue, 12 Dec 2017 07:32:10 +0000
        |From: "John Doe" <john.doe@mail.com>
        |To: "Phil Doe" <phil.doe@mail.com>
        |Content-Type: text/plain; charset=iso-8859-2
        |Content-Transfer-Encoding: quoted-printable
        |
        |This is a short line
        |This ia a very long line that shall be split by fws to multiple line to mak=
        |e sure that SMTP server won't complain on size of it
        |This line of text contains accented characters like =EC=B9=E8=F8=BE=FD=E1 t=
        |hat shall be encoded
        |. Dot starting line
        |--- end
        |
      """.stripMargin.lines.mkString("\r\n")
  }

  property("specified.transfer-encoding") = protect {

    SMTPClient.impl.encodeTextBody[Task](
      header = EmailHeader(
        subject = "Test Email"
        , date = ZonedDateTime.of(2017, 12, 12, 7, 32, 10, 0, ZoneId.of("GMT"))
        , from = EmailAddress("john.doe", "mail.com", Some("John Doe"))
        , to = EmailAddress("phil.doe", "mail.com", Some("Phil Doe"))
      ).contentTransferEncoding(TransferEncoding.Base64)
      , text = input1
      , emailHeaderCodec = emailCodec
      , mimeHeaderCodec = mimeCodec
    )
    .chunks.map(ByteVectorChunk.asByteVector)
    .runLog.map { _.reduce(_ ++ _).decodeUtf8.right.getOrElse("--ERR--") }
    .unsafeRun() ?=
    """Subject: Test Email
      |Date: Tue, 12 Dec 2017 07:32:10 +0000
      |From: "John Doe" <john.doe@mail.com>
      |To: "Phil Doe" <phil.doe@mail.com>
      |Content-Type: text/plain; charset=utf-8
      |Content-Transfer-Encoding: base64
      |
      |  VGhpcyBpcyBhIHNob3J0IGxpbmUNClRoaXMgaWEgYSB2ZXJ5IGxvbmcgbGluZSB0aGF0IHNoY
      |  WxsIGJlIHNwbGl0IGJ5IGZ3cyB0byBtdWx0aXBsZSBsaW5lIHRvIG1ha2Ugc3VyZSB0aGF0IF
      |  NNVFAgc2VydmVyIHdvbid0IGNvbXBsYWluIG9uIHNpemUgb2YgaXQNClRoaXMgbGluZSBvZiB
      |  0ZXh0IGNvbnRhaW5zIGFjY2VudGVkIGNoYXJhY3RlcnMgbGlrZSDEm8WhxI3FmcW+w73DoSB0
      |  aGF0IHNoYWxsIGJlIGVuY29kZWQNCi4gRG90IHN0YXJ0aW5nIGxpbmUNCi0tLSBlbmQNCg0KI
      |  CAgICAg
      |
      |""".stripMargin.lines.mkString("\r\n")
  }

}
