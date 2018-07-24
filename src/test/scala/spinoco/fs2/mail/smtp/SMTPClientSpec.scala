package spinoco.fs2.mail.smtp

import cats.effect.IO
import fs2._
import fs2.interop.scodec.ByteVectorChunk
import org.scalacheck._
import org.scalacheck.Prop._
import scodec.bits.ByteVector

import spinoco.fs2.mail.mime.SMTPResponse
import spinoco.fs2.mail.mime.SMTPResponse.Code

object SMTPClientSpec extends Properties("SMTPClient"){


  property("insert-dots") = forAll(Gen.choose(1, 40)) { (chunkSize: Int) =>

    Stream.chunk(ByteVectorChunk(ByteVector.view(
      """Line
        |.
        | .
        |x.
        |......
      """.stripMargin.lines.mkString("\r\n").getBytes))
    ).covary[IO]
    .chunkLimit(chunkSize).flatMap(ch => Stream.chunk(ch))
    .through(SMTPClient.impl.insertDotIfNeeded)
    .chunks.map { ch =>
      val bs = ch.toBytes
      ByteVector.view(bs.values, bs.offset, bs.size)
    }
    .compile.toVector
    .map(_.reduce(_ ++ _).decodeUtf8.right.toOption.getOrElse("").lines.mkString("\r\n"))
    .unsafeRunSync() ?=
      """Line
        |..
        | .
        |x.
        |.......
      """.stripMargin.lines.mkString("\r\n")
  }


  property("read-response.single-line") = forAll(Gen.choose(1, 200)) { (chunkSize: Int) =>

    Stream.chunk(ByteVectorChunk(ByteVector.view(
      """220 smtp.gmail.com ESMTP k185sm1251101wma.28 - gsmtp
        |
      """.stripMargin.lines.mkString("\r\n").getBytes
    ))).covary[IO]
    .chunkLimit(chunkSize).flatMap(ch => Stream.chunk(ch))
    .through(SMTPClient.impl.readResponse[IO])
    .compile.toVector.unsafeRunSync() ?= Vector(
      SMTPResponse(Code.Ready, "smtp.gmail.com ESMTP k185sm1251101wma.28 - gsmtp")
    )
  }

  property("read-response.multi-line") = forAll(Gen.choose(1, 200)) { (chunkSize: Int) =>

    Stream.chunk(ByteVectorChunk(ByteVector.view(
      """250-smtp.gmail.com at your service, [31.186.185.166]
        |250-SIZE 35882577
        |250-8BITMIME
        |250-AUTH LOGIN PLAIN XOAUTH2 PLAIN-CLIENTTOKEN OAUTHBEARER XOAUTH
        |250-ENHANCEDSTATUSCODES
        |250-PIPELINING
        |250-CHUNKING
        |250 SMTPUTF8
        |
      """.stripMargin.lines.mkString("\r\n").getBytes
    ))).covary[IO]
      .chunkLimit(chunkSize).flatMap(ch => Stream.chunk(ch))
      .through(SMTPClient.impl.readResponse[IO])
      .compile.toVector.unsafeRunSync() ?= Vector(
      SMTPResponse(Code.Completed, "smtp.gmail.com at your service, [31.186.185.166]")
      , SMTPResponse(Code.Completed, "SIZE 35882577")
      , SMTPResponse(Code.Completed, "8BITMIME")
      , SMTPResponse(Code.Completed, "AUTH LOGIN PLAIN XOAUTH2 PLAIN-CLIENTTOKEN OAUTHBEARER XOAUTH")
      , SMTPResponse(Code.Completed, "ENHANCEDSTATUSCODES")
      , SMTPResponse(Code.Completed, "PIPELINING")
      , SMTPResponse(Code.Completed, "CHUNKING")
      , SMTPResponse(Code.Completed, "SMTPUTF8")
    )
  }



  property("cram-md5-hash") = protect {
    SMTPClient.impl.computeCramMD5("PDI0NjA5LjEwNDc5MTQwNDZAcG9wbWFpbC5TcGFjZS5OZXQ+", "testPass") ?= Some(
      ByteVector.fromHex("84386e195950933f49ca7797a6d4d485").get
    )
  }





}
