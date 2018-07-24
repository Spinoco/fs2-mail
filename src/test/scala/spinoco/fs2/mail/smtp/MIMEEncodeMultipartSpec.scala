package spinoco.fs2.mail.smtp

import java.time.{ZoneId, ZonedDateTime}

import cats.effect.IO
import fs2._
import fs2.interop.scodec.ByteVectorChunk
import org.scalacheck.Properties
import org.scalacheck.Prop._
import scodec.bits.ByteVector

import spinoco.fs2.mail.interop.StringChunk
import spinoco.fs2.mail.mime.MIMEPart
import spinoco.protocol.mail.{EmailAddress, EmailHeader}
import spinoco.protocol.mail.header.codec.EmailHeaderCodec
import spinoco.protocol.mime.MediaType

object MIMEEncodeMultipartSpec extends Properties("MIMEEncodeMultipart") {

  val emailCodec = EmailHeaderCodec.codec(100 * 1024)
  val mimeCodec = EmailHeaderCodec.mimeCodec(10 * 1024)

  val binarySample1 = Stream.chunk(ByteVectorChunk(ByteVector.view(
    (for (i <- 0 until 1000) yield i.toByte).toArray
  )))

  val plaintext = Stream.chunk(StringChunk(
    """This is a short line
      |This ia a very long line that shall be split by fws to multiple line to make sure that SMTP server won't complain on size of it
      |This line of text contains accented characters like ěščřžýá that shall be encoded
      |. Dot starting line
      |--- end
      |
      |""".stripMargin.lines.mkString("\r\n")
  ))

   val htmlText = Stream.chunk(StringChunk(
     """<html>
       |  <body>
       |    <div>This is html text in multipart with longer line than allowed by mail body. Shall be splitted to smaller lines</div>
       |    <div>This contains international chars ěščřžý </div>
       |  </body>
       |</html>
       |""".stripMargin.lines.mkString("\r\n")
   ))

  property("alternative") = protect {

    SMTPClient.impl.encodeMimeBody(
      header = EmailHeader(
        subject = "Test Email"
        , date = ZonedDateTime.of(2017, 12, 12, 7, 32, 10, 0, ZoneId.of("GMT"))
        , from = EmailAddress("john.doe", "mail.com", Some("John Doe"))
        , to = EmailAddress("phil.doe", "mail.com", Some("Phil Doe"))
      )
      , body = MIMEPart.alternative(
        MIMEPart.html[IO](htmlText)
        , MIMEPart.plain[IO](plaintext)
        , boundary = "----boundary---"
      )
      , emailHeaderCodec = emailCodec
      , mimeHeaderCodec = mimeCodec
    )
    .chunks.map { ch =>
      val bs = ch.toBytes
      ByteVector.view(bs.values, bs.offset, bs.size)
    }
    .compile.toVector.map { _.reduce(_ ++ _).decodeUtf8.right.getOrElse("--ERR--") }
    .unsafeRunSync() ?=
    """Subject: Test Email
      |Date: Tue, 12 Dec 2017 07:32:10 +0000
      |From: "John Doe" <john.doe@mail.com>
      |To: "Phil Doe" <phil.doe@mail.com>
      |Content-Type: multipart/alternative; boundary="----boundary---"
      |Content-Transfer-Encoding: 8bit
      |
      |------boundary---
      |Content-Type: text/html; charset=utf-8
      |Content-Transfer-Encoding: quoted-printable
      |
      |<html>
      |  <body>
      |    <div>This is html text in multipart with longer line than allowed by ma=
      |il body. Shall be splitted to smaller lines</div>
      |    <div>This contains international chars =C4=9B=C5=A1=C4=8D=C5=99=C5=BE=
      |=C3=BD </div>
      |  </body>
      |</html>
      |------boundary---
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
      |------boundary-----
      |
      |""".stripMargin.lines.mkString("\r\n")


  }

  property("alternative.attachment") = protect {
      SMTPClient.impl.encodeMimeBody(
        header = EmailHeader(
          subject = "Test Email"
          , date = ZonedDateTime.of(2017, 12, 12, 7, 32, 10, 0, ZoneId.of("GMT"))
          , from = EmailAddress("john.doe", "mail.com", Some("John Doe"))
          , to = EmailAddress("phil.doe", "mail.com", Some("Phil Doe"))
        )
        , body = MIMEPart.multipart[IO](
          subtype = "mixed"
          , parts = Stream.emit(
            MIMEPart.alternative(
              MIMEPart.html[IO](htmlText)
              , MIMEPart.plain[IO](plaintext)
              , boundary = "----alt-boundary---"
            )
          ) ++ Stream.emit(
            MIMEPart.file[IO](
              "jpeg-picture"
              , "awesome.jpeg"
              , MediaType.`image/jpeg`
              , binarySample1
            )
          )
          , boundary = "---mixed-boundary---"
        )
        , emailHeaderCodec = emailCodec
        , mimeHeaderCodec = mimeCodec
      )
      .chunks.map { ch =>
        val bs = ch.toBytes
        ByteVector.view(bs.values, bs.offset, bs.size)
      }
      .compile.toVector.map { _.reduce(_ ++ _).decodeUtf8.right.getOrElse("--ERR--") }
      .unsafeRunSync() ?=
      """Subject: Test Email
        |Date: Tue, 12 Dec 2017 07:32:10 +0000
        |From: "John Doe" <john.doe@mail.com>
        |To: "Phil Doe" <phil.doe@mail.com>
        |Content-Type: multipart/mixed; boundary="---mixed-boundary---"
        |Content-Transfer-Encoding: 8bit
        |
        |-----mixed-boundary---
        |Content-Type: multipart/alternative; boundary="----alt-boundary---"
        |Content-Transfer-Encoding: 8bit
        |
        |------alt-boundary---
        |Content-Type: text/html; charset=utf-8
        |Content-Transfer-Encoding: quoted-printable
        |
        |<html>
        |  <body>
        |    <div>This is html text in multipart with longer line than allowed by ma=
        |il body. Shall be splitted to smaller lines</div>
        |    <div>This contains international chars =C4=9B=C5=A1=C4=8D=C5=99=C5=BE=
        |=C3=BD </div>
        |  </body>
        |</html>
        |------alt-boundary---
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
        |------alt-boundary-----
        |
        |-----mixed-boundary---
        |Content-Type: image/jpeg
        |Content-Transfer-Encoding: base64
        |Content-ID: <jpeg-picture>
        |Content-Disposition: attachment; filename="awesome.jpeg"
        |
        |  AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1N
        |  jc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG
        |  1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqO
        |  kpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2Nna
        |  29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+/wABAgMEBQYHCAkKCwwNDg8QE
        |  RITFBUWFxgZGhscHR4fICEiIyQlJicoKSorLC0uLzAxMjM0NTY3ODk6Ozw9Pj9AQUJDREVGR0
        |  hJSktMTU5PUFFSU1RVVldYWVpbXF1eX2BhYmNkZWZnaGlqa2xtbm9wcXJzdHV2d3h5ent8fX5
        |  /gIGCg4SFhoeIiYqLjI2Oj5CRkpOUlZaXmJmam5ydnp+goaKjpKWmp6ipqqusra6vsLGys7S1
        |  tre4ubq7vL2+v8DBwsPExcbHyMnKy8zNzs/Q0dLT1NXW19jZ2tvc3d7f4OHi4+Tl5ufo6err7
        |  O3u7/Dx8vP09fb3+Pn6+/z9/v8AAQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAhIi
        |  MkJSYnKCkqKywtLi8wMTIzNDU2Nzg5Ojs8PT4/QEFCQ0RFRkdISUpLTE1OT1BRUlNUVVZXWFl
        |  aW1xdXl9gYWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp7fH1+f4CBgoOEhYaHiImKi4yNjo+Q
        |  kZKTlJWWl5iZmpucnZ6foKGio6SlpqeoqaqrrK2ur7CxsrO0tba3uLm6u7y9vr/AwcLDxMXGx
        |  8jJysvMzc7P0NHS09TV1tfY2drb3N3e3+Dh4uPk5ebn6Onq6+zt7u/w8fLz9PX29/j5+vv8/f
        |  7/AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ
        |  1Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWpr
        |  bG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6Cho
        |  qOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2N
        |  na29zd3t/g4eLj5OXm5w==
        |
        |-----mixed-boundary-----
        |
        |""".stripMargin.lines.mkString("\r\n")
  }

}
