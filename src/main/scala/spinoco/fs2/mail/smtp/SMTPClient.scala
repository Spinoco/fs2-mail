package spinoco.fs2.mail.smtp



import fs2._
import fs2.async.mutable.Semaphore
import fs2.io.tcp.Socket
import fs2.util.{Async, Effect}
import fs2.util.syntax._
import scodec.{Attempt, Codec}
import scodec.bits.ByteVector
import shapeless.tag.@@
import spinoco.fs2.mail.encoding
import spinoco.fs2.mail.interop.ByteVectorChunk
import spinoco.fs2.mail.mime.MIMEPart.{MultiPart, SinglePart}
import spinoco.fs2.mail.mime.SMTPResponse.Code
import spinoco.fs2.mail.mime.{MIMEPart, SMTPError, SMTPResponse}
import spinoco.protocol.mail.header.codec.EmailHeaderCodec
import spinoco.protocol.mail.{EmailAddress, EmailHeader}
import spinoco.protocol.mail.header._
import spinoco.protocol.mail.mime.TransferEncoding.StandardEncoding
import spinoco.protocol.mail.mime.{MIMEHeader, TransferEncoding}
import spinoco.protocol.mime.{ContentType, MIMECharset, MediaType}

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/**
  * SMTP Client implemented according to RFC 5321
  */
trait SMTPClient[F[_]] {

  /** Contains server's welcome message, i.e. 220 foo.com Simple Mail Transfer Service Ready **/
  def serverId: F[String]

  /**
    * Connects with SMTP server. Returns a list of supported extensions of this server.
    * @param domain domain name to present on HELO/EHLO
    * @return
    */
  def connect(domain: String): F[Seq[String]]

  /** login to SMTP server with supplied user and password. This is equivalent to AUTH LOGIN **/
  def login(userName: String, password: String): F[Unit]

  /** login to SMP server with supplied credentials. This is equivalent to AUTH PLAIN command **/
  def loginPlain(authorizationId: String, authenticationId: String, pass: String): F[Unit]

  /** login to SMTP server with CRAM MD5. This is equivalent to AUTH CRAM-MD5 command **/
  def loginCramMD5(userName: String, password: String): F[Unit]

  /**
    * Sends simple text message where `text` forms a body of the message.
    * Encodes as QuotedPrintable, UTF-8 encoding
    * @param from       source address presented to server.
    * @param recipient  Recipient for this email
    * @param header     Header present to server
    * @param text       text to send as body
    * @return nonempty if sent was ok, Some(result) otherwise
    */
  def sendText(
    from: String @@ EmailAddress
    , recipient: String @@ EmailAddress
    , header: EmailHeader
    , text: Stream[F, Char]
  ) : F[Option[SMTPError]]

  /**
    * Send the mail where body is supplied as MIME Part (eventually multiple)
    * @param from     source address presented to server
    * @param recipient  Recipient for this email
    * @param header   Header of email message
    * @param body     Body of the email (MIME).
    * @return  nonempty if sent was ok, Some(result) otherwise
    */
  def send(
    from: String @@ EmailAddress
    , recipient: String @@ EmailAddress
    , header: EmailHeader
    , body: MIMEPart[F]
  ) : F[Option[SMTPError]]

  /**
    * Terminates SMTP Session with the server.
    */
  def quit: F[Unit]

}


object SMTPClient {

  /**
    * Creates SMTP client. Once stream is run, the client connects to server and expects server to
    * send its welcome message (i.e. 220 foo.com, SMTP server Ready)
    * @param socket   Socket to use fro SMTP Connection to server
    */
  def mk[F[_]](
    socket: Socket[F]
    , initialHandshakeTimeout: FiniteDuration
    , sendTimeout: FiniteDuration
    , emailHeaderCodec: Codec[EmailHeader] = EmailHeaderCodec.codec(100 * 1024) // 100K max header size
    , mimeHeaderCodec: Codec[MIMEHeader] = EmailHeaderCodec.mimeCodec(10 * 1024) // 10K for mime shall be enough
  )(implicit F: Async[F]): Stream[F, SMTPClient[F]] = {
    implicit val socket_ = socket
    Stream.eval(async.semaphore(0)) flatMap { sending =>
    Stream.eval(F.ref[String]) flatMap { serverIdRef =>


      // initialize the SMTP client to await first line (welcome) from the server.
      // also when this terminates the sending semaphore is open.
      def initialize =
        (impl.initialHandshake(initialHandshakeTimeout) flatMap serverIdRef.setPure) >>
        sending.increment

      // sends requests and collects any response
      implicit val sendRequest = impl.sendRequest(sendTimeout, sending) _

      Stream.eval(F.start(initialize)) map { _ =>
      new SMTPClient[F] {
        def serverId: F[String] = serverIdRef.get
        def connect(domain: String): F[Seq[String]] =
          sendRequest(impl.connect(domain)).map(_.map(_.data))

        def login(userName: String, password: String): F[Unit] =
          impl.login(userName, password)

        def loginPlain(authorizationId: String, authenticationId: String, pass: String) =
          impl.loginPlain(authorizationId, authenticationId, pass)

        def loginCramMD5(userName: String, password: String) =
          impl.loginCramMD5(userName, password)

        def sendText(from: @@[String, EmailAddress], recipient: String @@ EmailAddress, header: EmailHeader, text: Stream[F, Char]): F[Option[SMTPError]] =
          impl.sendMail(from, recipient, impl.encodeTextBody(header, text, emailHeaderCodec, mimeHeaderCodec))

        def quit: F[Unit] = sendRequest(impl.quit) as (())

        def send(from: @@[String, EmailAddress], recipient: String @@ EmailAddress, header: EmailHeader, body: MIMEPart[F]): F[Option[SMTPError]] =
          impl.sendMail(from, recipient, impl.encodeMimeBody(header, body, emailHeaderCodec, mimeHeaderCodec))
      }}

    }}
  }


  object impl {

    /**
      * Performs initial handshake, that is expects server to deliver single line of welcome message.
      * Terminates once that message was delivered or when timeout is exceeded.
      * Also may terminate with failure
      * @param socket
      * @tparam F
      * @return
      */
    def initialHandshake[F[_]](
      timeout: FiniteDuration
    )(implicit socket: Socket[F], F: Effect[F]): F[String] = {
      (socket.reads(1024, Some(timeout)) through readResponse).runLog flatMap { resp =>
        resp.headOption match {
          case Some(resp@SMTPResponse(code, text)) =>
            if (code == Code.Ready) F.pure(text)
            else F.fail(new SMTPError(resp))
          case None => F.fail(new Throwable("Expected at least one response with code 220, got None"))
        }
      }
    }


    private val crlf: ByteVector = ByteVector.view("\r\n".getBytes)

    /**
      * Read response lines of input as they come in. Lines are expected to be terminated with \r\n and
      * starts with result code, following informative text.
      *
      * If the code follows with - (i.e. 250-SIZE) this will not terminate and will start other line.
      * It the code follows with spacce, then this terminates.
      *
      * It assumes the server never sends more data than very last \r\n, as the other data received will be discarded.
      *
      */
    def readResponse[F[_]]: Pipe[F, Byte, SMTPResponse] = {
      def go(buff: ByteVector): Handle[F, Byte] => Pull[F, String, Unit] = {
        _ receiveOption {
          case Some((ch, h)) =>
            val bv = buff ++ ByteVectorChunk.asByteVector(ch)
            val idx = bv.indexOfSlice(crlf)
            if (idx < 0) go(bv)(h)
            else {
              val (line, t) = bv.splitAt(idx)
              line.decodeAscii match {
                case Right(s) => Pull.output1(s) >> go(ByteVector.empty)(h.push(ByteVectorChunk(t.drop(crlf.size))))
                case Left(err) => Pull.fail(new Throwable(s"Failed to decode data from server: $bv", err))
              }
            }
          case None => Pull.done
        }
      }
      _.pull(go(ByteVector.empty)).flatMap { line =>
        if (line.length < 4) Stream.fail(new Throwable(s"Failed to process server response, server response must have size of at least 4 characters. got $line"))
        else {
          Try(Code(line.take(3).toInt)).fold(
            err => Stream.fail(new Throwable(s""))
            , { code =>
              val out = Stream.emit(Some(SMTPResponse(code, line.drop(4))))
              if (line(3) == '-') out
              else out ++ Stream.emit(None)
          })

        }
      }.unNoneTerminate
    }

    /**
      * Sends given lines of data to server (adjusting them according to RFC transparency
      * Note that supplied chunks must be exactly one size of the length
      * Returns any response of the server as received. This will accumulate all the lines receved from the
      * server, i.e. on EHLO command
      * @param data     Data to sent
      * @param timeout  Timeout to wait for server to respond
      * @param sending  A semaphore preventing concurrent sends.
      * @tparam F
      * @return
      */
    def sendRequest[F[_]](
       timeout: FiniteDuration
      , sending: Semaphore[F]
    )(data: Stream[F, Byte])(implicit socket: Socket[F], F: Effect[F]): F[Seq[SMTPResponse]] = {
      sending.decrement >>
      data.to(socket.writes()).run >>
      socket.reads(1024, Some(timeout)).through(readResponse).runLog.attempt flatMap {
        case Right(rslt) => sending.increment as rslt
        case Left(err) => sending.increment >> F.fail(err)
      }
    }


    /** prepares single stream chunk of command data  terminated by crlf to send **/
    def command[F[_]](s: String): Stream[F, Byte] =
      Stream.chunk(ByteVectorChunk(ByteVector.view((s + "\r\n").getBytes)))


    /** issues EHLO $domain command **/
    def connect[F[_]](domain: String): Stream[F, Byte] = command(s"EHLO $domain")


    /** issues QUIT command **/
    def quit[F[_]]: Stream[F, Byte] = command(s"QUIT")

    /** encodes to base64 termiated with crlf **/
    def toBase64Line(s: String): ByteVector =
      ByteVector.view((ByteVector.view(s.getBytes).toBase64 + "\r\n").getBytes)

    /**
      * Performs AUTH LOGIN command .
      * UserName and password are Base64 encdoed and sent to server
      * @param userName Name of the user
      * @param password password
      */
    def login[F[_]](userName: String, password: String)(implicit send: Stream[F, Byte] => F[Seq[SMTPResponse]], F: Effect[F]): F[Unit] = {

      def failed(tag: String, resp: Seq[SMTPResponse]): F[Unit] =
        F.fail(new Throwable(s"Unexpected response during login [$tag]: $resp"))

      def continues(result: Seq[SMTPResponse]):Boolean =
        result.exists(_.code == Code.Continue)

      send(command("AUTH LOGIN")) flatMap { loginResult =>
        if (! continues(loginResult)) failed("login", loginResult)
        else send(Stream.chunk(ByteVectorChunk(toBase64Line(userName)))) flatMap { userNameResult =>
          if (! continues(userNameResult)) failed("userName", userNameResult)
          else send(Stream.chunk(ByteVectorChunk(toBase64Line(password)))) flatMap { passwordResult =>
            if (! passwordResult.exists(_.code == Code.AuthAccepted)) failed("password", passwordResult)
            else F.pure(())
          }
        }
      }

    }

    private val zero = ByteVector.view(Array[Byte](0))

    /**
      * Performs AUTH PLAIN login command.
      *
      * @param authorizationId    Authorization id
      * @param authenticationId   Authenitcation id
      * @param pass               Password
      */
    def loginPlain[F[_]](
      authorizationId: String
      , authenticationId: String
      , pass: String
    )(implicit send: Stream[F, Byte] => F[Seq[SMTPResponse]], F: Effect[F]): F[Unit] = {
      val encoded = ByteVector.view(authorizationId.getBytes) ++ zero ++
                    ByteVector.view(authenticationId.getBytes) ++ zero ++
                    ByteVector.view(pass.getBytes)
      send(command(s"AUTH LOGIN ${encoded.toBase64}")) flatMap { loginResult =>
        if (! loginResult.exists(_.code == Code.AuthAccepted)) F.fail(new Throwable(s"Unexpected response during plain login: $loginResult"))
        else F.pure(())
      }
    }


    /** logins via cram-md5 **/
    def loginCramMD5[F[_]](userName: String, password: String)(implicit send: Stream[F, Byte] => F[Seq[SMTPResponse]], F: Effect[F]): F[Unit] = {
      send(command(s"AUTH CRAM-MD5")) flatMap { loginResult =>
        loginResult.find(_.code == Code.Continue) match {
          case None => F.fail(new Throwable(s"Unexpected response during cram-md5 [nonce]: $loginResult"))
          case Some(resp) => computeCramMD5(resp.data, password) match {
            case None => F.fail(new Throwable(s"Failed to compute cram-md5 data invalid nonce? : $resp"))
            case Some(cramComputed) => send(command(ByteVector.view(s"$userName ${cramComputed.toHex}".getBytes).toBase64)) flatMap { authResult =>
              if (!authResult.exists(_.code == Code.AuthAccepted)) F.fail(new Throwable(s"Unexpected response during cram-md5 [auth]: $authResult"))
              else F.pure(())
            }
          }
        }
      }
    }

    /** computes cram md5 hash, yields to None on invalid Nonce. **/
    def computeCramMD5(nonce: String, secret: String): Option[ByteVector] = {
      val secretBytes = ByteVector.view(secret.getBytes)
      val key = // RFC 2104 limit on key is 64 bytes, if greater digest has to be used instead of the key
        if (secretBytes.length > 64) secretBytes.digest("MD5")
        else secretBytes

      // create ipad/opad with magic keys
      val ipad = key.padTo(64).map(b => (b ^ 0x36).toByte)
      val opad = key.padTo(64).map(b => (b ^ 0x5c).toByte)

      ByteVector.fromBase64(nonce).map { nonceBytes =>
        (opad ++ (ipad ++ nonceBytes).digest("MD5")).digest("MD5")
      }
    }

    /** assures mail is always in <> **/
    def wrapMail(email: String @@ EmailAddress): String = {
      val trimmed = email.trim
      if (trimmed.startsWith("<")) trimmed else s"<$trimmed>"
    }

    /** wraps from email **/
    def mailFrom[F[_]](email: String @@ EmailAddress): Stream[F, Byte] =
      command(s"MAIL FROM:${wrapMail(email)}")


    /** wraps to email **/
    def rcptTo[F[_]](email: String @@ EmailAddress): Stream[F, Byte] =
      command(s"RCPT TO:${wrapMail(email)}")

    /** signal tx failed **/
    def txFail[F[_]](result: Seq[SMTPResponse])(implicit F: Effect[F]): F[Option[SMTPError]] = {
      result.headOption match {
        case Some(resp) => F.pure(Some(SMTPError(resp)))
        case None =>   F.fail(new Throwable("No response received"))
      }
    }

    val dotLine = ByteVector.view("\r\n.".getBytes)
    val dotDotLine = ByteVector.view("\r\n..".getBytes)
    val cr = ByteVector.view("\r".getBytes)

    /**
      * SMTP RFC requires that each line starting with . (dot) shall be prepended with another dot,
      * unless the don indicates end of input for DATA command section.
      *
      * This facilites that requirement.
      */
    def insertDotIfNeeded[F[_]]: Pipe[F, Byte, Byte] = {
      def go(buff: ByteVector): Handle[F, Byte] => Pull[F, Byte, Unit] = {
        _.receiveOption {
          case Some((ch, h)) =>
            val bv = buff ++ ByteVectorChunk.asByteVector(ch)
            val idx = bv.indexOfSlice(dotLine)
            if (idx < 0) {
              if (bv.size < 2) {
                if (bv == cr) go(cr)(h)
                else Pull.output(ByteVectorChunk(bv)) >> go(ByteVector.empty)(h)
              } else {
                val tail = bv.takeRight(2)
                if (tail == crlf) Pull.output(ByteVectorChunk(bv.take(bv.size - 2))) >> go(crlf)(h)
                else if (tail.drop(1) == cr) Pull.output(ByteVectorChunk(bv.take(bv.size -1))) >> go(cr)(h)
                else Pull.output(ByteVectorChunk(bv)) >> go(ByteVector.empty)(h)
              }
            } else {
              val (head, t) = bv.splitAt(idx)
              Pull.output(ByteVectorChunk(head ++ dotDotLine)) >> go(ByteVector.empty)(h.push(ByteVectorChunk(t.drop(dotLine.size))))
            }

          case None => Pull.output(ByteVectorChunk(buff))
        }
      }
      _.pull(go(ByteVector.empty))
    }

    // end of email content streaming
    private val EndOfContent: ByteVectorChunk =
      ByteVectorChunk(ByteVector.view("\r\n.\r\n".getBytes))

    /**
      * Sends mail from given address, header and body.
      * @param from         Source address
      * @param recipient    Recipient of the email
      * @param content      Content of the email, including the header
      * @tparam F
      * @return
      */
    def sendMail[F[_]](
      from: String @@ EmailAddress
      , recipient: String @@ EmailAddress
      , content: Stream[F, Byte]
    )(implicit
      send: Stream[F, Byte] => F[Seq[SMTPResponse]]
      , F: Effect[F]
    ) : F[Option[SMTPError]] = {
      send(mailFrom(from)) flatMap { fromResult =>
        if (! fromResult.exists(_.code == Code.Completed)) txFail(fromResult)
        else send(rcptTo(recipient)) flatMap { rcptResult =>
          if (! rcptResult.exists(_.code == Code.Completed)) txFail(rcptResult)
          else send(command("DATA")) flatMap { dataResult =>
            if(! dataResult.exists(_.code == Code.StartMail)) txFail(dataResult)
            else {
              send(content.through(insertDotIfNeeded) ++ Stream.chunk(EndOfContent)) flatMap { result =>
                if (! result.exists(_.code == Code.Completed)) txFail(result)
                else F.pure(None)
              }
            }

          }
        }
      }
    }

    private val crlfChunk = Stream.chunk(ByteVectorChunk(crlf))

    /**
      * Encodes mail body based on supplied body and header.
      * @param header             Header to encode before the message.
      *                           Note this will strp any Content-Type header from the email header and instead this
      *                           will use Content-Type from the supplied body.
      * @param body               Body to send to client
      * @param emailHeaderCodec   Codec for the email header
      * @param mimeHeaderCodec    Codec for the mime header
      * @tparam F
      * @return
      */
    def encodeMimeBody[F[_]](
      header: EmailHeader
      , body: MIMEPart[F]
      , emailHeaderCodec: Codec[EmailHeader]
      , mimeHeaderCodec: Codec[MIMEHeader]
    ): Stream[F, Byte] = {
      def encodeHeader[A](label: String, a: A, codec: Codec[A]): Stream[F, Byte] = {
        codec.encode(a) match {
          case Attempt.Successful(bits) =>
            Stream.chunk(ByteVectorChunk(bits.bytes))

          case Attempt.Failure(err) =>
            Stream.fail(new Throwable(s"Failed to encode [$label]: $err ($a)"))
        }
      }

      // multi mime codec shall encode its header, and then for each of its part
      // it shall encode the header, then crlf, and then for each of its
      // body parts the boundary followed by encoded part.
      // after each part the boundary must be encoded too
      def encodeMulti(multi: MultiPart[F]): Stream[F, Byte] = {
        lazy val boundaryChunk: Stream[F, Byte] = Stream.chunk(ByteVectorChunk(crlf ++ ByteVector.view(multi.boundary.getBytes) ++ crlf))

        encodeHeader("mime multi-part header", multi.header, mimeHeaderCodec) ++
        boundaryChunk ++ multi.parts.flatMap { part =>
          encode(part) ++ boundaryChunk
        }
      }

      // encodes single mime part.
      // this encodes header followed by crlf and then actual body
      def encodeSingle(single: SinglePart[F]): Stream[F, Byte] = {
        encodeHeader("mime single-part header", single.header, mimeHeaderCodec) ++ crlfChunk ++ single.data
      }

      def encode(part: MIMEPart[F]): Stream[F, Byte] = {
        part match {
          case single: SinglePart[F] => encodeSingle(single)
          case multi: MultiPart[F] => encodeMulti(multi)
        }
      }

      val updatedHeader =
        header.copy(fields = header.fields.filterNot {
          case _: `Content-Type` => true
          case _: `Content-Transfer-Encoding` => true
          case _ => false
        })

      encodeHeader("email header", updatedHeader, emailHeaderCodec) ++ encode(body)
    }

    private val plainTextContentType = ContentType.TextContent(MediaType.`text/plain`, Some(MIMECharset.`UTF-8`))

    /**
      * Encodes the supplied character body as single MIME text email body.
      * The type of the charset and encoding is queried from email header, if not present then
      * `text/plain` and UTF8 will be used.
      *
      * @param header               Header
      * @param text                 Text to encode
      * @param emailHeaderCodec     codec to encode email header
      * @param mimeHeaderCodec      codec to encode mime header
      */
    def encodeTextBody[F[_]: Effect](
      header: EmailHeader
      , text: Stream[F, Char]
      , emailHeaderCodec: Codec[EmailHeader]
      , mimeHeaderCodec: Codec[MIMEHeader]
    ): Stream[F, Byte] = {
      val textContent =
        header.get[`Content-Type`]
        .map(_.tpe)
        .collectFirst { case text: ContentType.TextContent => text }
        .getOrElse(plainTextContentType)

      val mimeCharset =
        textContent.charset.getOrElse(MIMECharset.`UTF-8`)

      val transferEncoding =
        header.get[`Content-Transfer-Encoding`]
        .map(_.value)
        .collectFirst { case enc: StandardEncoding => enc }
        .getOrElse(TransferEncoding.QuotedPrintable)

      MIMECharset.asJavaCharset(mimeCharset) match {
        case Attempt.Successful(chset) =>
          val mime = MIMEPart.SinglePart(
            header = MIMEHeader(List(`Content-Type`(textContent), `Content-Transfer-Encoding`(transferEncoding)))
            , data = text through encoding.charset.encode(chset) through MIMEPart.textEncoder(transferEncoding)
          )
          encodeMimeBody(header, mime, emailHeaderCodec, mimeHeaderCodec)

        case Attempt.Failure(err) =>
          Stream.fail(new Throwable(s"Invalid charset requested: $mimeCharset : $err"))
      }
    }


  }

}
