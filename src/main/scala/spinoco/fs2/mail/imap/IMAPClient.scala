package spinoco.fs2.mail.imap


import java.nio.charset.{Charset, StandardCharsets}

import fs2._
import fs2.async.mutable.Semaphore
import fs2.io.tcp.Socket
import fs2.util.{Async, Catchable, Effect, Monad}
import fs2.util.syntax._
import scodec.{Attempt, Codec}
import scodec.bits.{BitVector, ByteVector}
import shapeless.tag
import shapeless.tag.@@
import spinoco.fs2.mail.imap.IMAPCommand._
import spinoco.fs2.mail.interop.ByteVectorChunk
import spinoco.fs2.mail.encoding.{base64, charset, quotedPrintable}
import spinoco.protocol.mail.EmailHeader
import spinoco.protocol.mail.header.codec.EmailHeaderCodec
import spinoco.protocol.mail.imap.codec.IMAPBodyPartCodec

import scala.collection.immutable.NumericRange
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex



/**
  * Simple IMAP client that allows to exchange messages(EMAIL) with server via IMAP protocol.
  *
  * Note that operations on the IMAP client must be executed in sequence (i.e. you sohould not
  * concurrently execute fetching of two body streams). Although concurrent operations are safe,
  * only one of them will be allowed at any given time.
  *
  *
  */
trait IMAPClient[F[_]] {

  /**
    * Performs LOGIN to IMAP server with supplied credentials.
    *
    * Returns  on left when login was not succesfull and on right with capabilities supported.
    *
    * @param userName   Name of the user
    * @param password   Users password
    * @return
    */
  def login(userName: String, password: String): F[IMAPResult[Seq[String]]]


  /**
    * Performs LOGOUT as per RFC3501 6.1.3. Note as result of this command the connection with the
    * server will be closed, and further commands to server will fail.
    * Note that logout is automatically executed when the stream with IMAPClient terminates.
    */
  def logout: F[Unit]


  /**
    * When invoked this will return response to IMAPv4 CAPABILITY command as defined in RFC3501 6.1.1
    *
    * @return
    */
  def capability: F[IMAPResult[Seq[String]]]


  /**
    * Perform the SELECT command as per RFC3501 6.3.1.
    * @param mailbox   Name of the mailbox to select
    */
  def select(mailbox: String @@ MailboxName): F[IMAPResult[IMAPMailboxStatus]]


  /**
    * Perform the EXAMINE command as per RFC3501 6.3.1.
    * @param mailbox Name of the mailbox for examine command
    * @return
    */
  def examine(mailbox: String @@ MailboxName): F[IMAPResult[IMAPMailboxStatus]]

  /**
    * Returns a list of mailboxes, that are available to logged in user
    * @return
    */
  def list(reference: String, wildcardName: String): F[IMAPResult[Seq[IMAPMailbox]]]

  /**
    * Perfrom seach of the messages in selected mailbox as per RFC3501 6.4.4
    * @param term     term to execute search with
    * @param charset  charset, when not specified, ASCII is used
    * @return
    */
  def search(term: IMAPSearchTerm, charset: Option[String] = None): F[IMAPResult[Seq[Long @@ MailUID]]]


  /**
    * Fetches email headers specified from the currently selected mailbox (RFC3501 6.4.5), given the specified
    * range of emails.
    *
    * @param range of email to return
    * @return
    */
  def emailHeaders(range: NumericRange[Long]): F[Vector[IMAPEmailHeader]]


  /**
    * Allows to fetch body structue of specified email.
    */
  def bodyStructureOf(uid: Long @@ MailUID): F[IMAPResult[Seq[EmailBodyPart]]]

  /**
    * Allows to fetch bytes of given mime binary part
    * @param uid    Id of message
    * @param part   Binary part specification. The data in binary part specification will be used
    *               to parse conent to stream of bytes.
    * @return
    */
  def bytesOf(uid: Long @@ MailUID, part: EmailBodyPart.BinaryPart): Stream[F, Byte]

  /**
    * Allows to fetch textual representation of given mime part
    * @param uid    Id of message
    * @param part   Textual part specification. The data in specification will be used to decode
    *               text to resulting stream of strings.
    * @return
    */
  def textOf(uid: Long @@ MailUID, part: EmailBodyPart.TextPart): Stream[F, Char]

}

/**
  * Created by pach on 11/10/17.
  */
object IMAPClient {


  /**
    * Creates imap client.
    *
    * @param socket           Socket (TCP) to use. This may be either TCP or TLS/TCP socket.
    * @param maxReadBytes     Read that much bytes in single chunk from the IMAPv4 server via supplied socket
    * @param bufferLines      Read that much lines of IMAP protocol before requiring client to consume content to read more.
    */
  def mk[F[_]](
    socket: Socket[F]
    , maxReadBytes: Int = 32*1024
    , bufferLines: Int = 64
    , emailHeaderCodec: Codec[EmailHeader] = EmailHeaderCodec.codec(100 * 1024) // 100K max header size
  )(implicit F: Async[F]): Stream[F, IMAPClient[F]] = {
    import impl._

    Stream.eval(async.signalOf[F, Boolean](false)) flatMap { terminated =>
    Stream.eval(Async.refOf[F, Long](0l)) flatMap { idxRef =>
    Stream.eval(async.semaphore(0)) flatMap { requestSemaphore =>
    Stream.eval(async.boundedQueue[F, IMAPData](bufferLines)) flatMap { incomingQ =>

      val received =
        (
          socket.reads(maxReadBytes, None) through
          lines through
          incomingQ.enqueue
        ) interruptWhen terminated

      val handshakeInitial =
        incomingQ.dequeue through concatLines takeWhile { ! _.startsWith("* OK") } onFinalize (requestSemaphore.increment)

      def send(line: String): F[Unit] =
        socket.write(Chunk.bytes(line.getBytes), None)

      val request = requestCmd(idxRef, requestSemaphore, incomingQ.dequeue, send) _

      val client =
        new IMAPClient[F] {
          def login(userName: String, password: String) =
            shortContent(request(LoginPlainText(userName, password)))(parseLogin[F])

          def logout =
            shortContent(request(Logout)) { _ => F.pure(()) } as (())

          def capability =
            shortContent(request(Capability))(parseCapability[F])

          def select(mailbox: @@[String, MailboxName]) =
            shortContent(request(Select(mailbox)))(parseSelect[F])

          def examine(mailbox: @@[String, MailboxName]) =
            shortContent(request(Examine(mailbox)))(parseSelect[F])

          def list(reference: String, wildcardName: String): F[IMAPResult[Seq[IMAPMailbox]]] =
            shortContent(request(ListMailbox(reference, wildcardName)))(parseMailboxList[F])

          def search(term: IMAPSearchTerm, charset: Option[String] = None): F[IMAPResult[Seq[Long @@ MailUID]]] =
            shortContent(request(Search(charset, term)))(parseSearchResult[F])

          def emailHeaders(range: NumericRange[Long]): F[Vector[IMAPEmailHeader]] =
            rawContent(request(Fetch(range, Seq(IMAPFetchContent.UID, IMAPFetchContent.Body(BodySection.HEADER)))))
            .through(fetchLog)
            .through(mkEmailHeader(emailHeaderCodec))
            .runFold(Vector.empty[IMAPEmailHeader])(_ :+ _)

          def bodyStructureOf(uid: @@[Long, MailUID]): F[IMAPResult[Seq[EmailBodyPart]]] =
            shortContent(request(Fetch(NumericRange(uid:Long, uid:Long, 1), Seq(IMAPFetchContent.BODYSTRUCTURE))))(parseBodyStructure[F])

          def bytesOf(uid: @@[Long, MailUID], part: EmailBodyPart.BinaryPart): Stream[F, Byte] = {
            val content = IMAPFetchContent.Body(BodySection(part.partId))
            rawContent(request(Fetch(NumericRange(uid: Long, uid: Long, 1), Seq(content)))) through
            fetchBytesOf(0, content.content, part.tpe.fields.encoding)
          }

          def textOf(uid: @@[Long, MailUID], part: EmailBodyPart.TextPart): Stream[F, Char] = {
            val content = IMAPFetchContent.Body(BodySection(part.partId))
            rawContent(request(Fetch(NumericRange(uid: Long, uid: Long, 1), Seq(content)))) through
            fetchTextOf(0, content.content, part.tpe.fields.encoding, part.charsetName)
          }
}

      concurrent.join(Int.MaxValue)(Stream(
        Stream.emit(client)
        , received.drain
        , handshakeInitial.drain
      ))
      .interruptWhen(terminated)
      .onFinalize(terminated.set(true))
    }}}}
  }



  object impl {

    type RequestResult[F[_]] = Stream[F, Either[String, Stream[F, IMAPData]]]

    /**
      * Helps to abstract over the results received from IMAP protocol.
      *
      * Essentially solves the situation of resolving {sz}\r\n pattern, where this pattern has to be replaced
      * by `sz` of bytes in various formats and encodings (i.e. MIME)
      */
    sealed trait IMAPData

    /** text, that is known to be represented by characters. It may be whole line terminated by crlf ( in that case `s` is w/o crlf) **/
    final case class IMAPText(s: String) extends IMAPData

    /**
      * Streamed data content, that was created as replacement for {sz}\r\n macro containing all content.
      * Note that this may emit more bytes as they come in, where the `data` size will never exceed size of `sz`
      * in macro nor the max size of chunk as defined by underlying TCP socket.
      */
    final case class IMAPBytes(data: ByteVector) extends IMAPData



    /**
      * Executes supplied command against the request, expecting the result of the command collected in resulting stream.
      *
      * Result is the stream, that either emits once on left with error description, if instead of any content the failure
      * of the commmand was recived. Otherwise, the client emits on right, Strema of the data that were retruned by server.
      * Note that the data retruned on right must be consumed, before command lock is released.
      *
      * @param idxRef             Ref containing IDX that will be used to generate unique idx, that will be used to pair the response with command.
      *                           Also note that client will only arrow one command at a time to be run against the server.
      *                           Command will not complete before the resposne from the server is received.
      *
      * @param requestSemaphore   Semaphore, that guards only one request to be peformed at any given time. Even though IMAPv4 spec
      *                           allows for multiple concurrent commands in certain scenarios, this client strictly enforces sequential
      *                           command execution.
      *
      * @param fromServer         A stream of responses (lines) fromserver immedaitelly following the command executed.
      *
      * @param toServer           Will send one line to server
      *
      * @param cmd
      * @param F
      * @tparam F
      * @return
      */
    def requestCmd[F[_]](
      idxRef: Async.Ref[F, Long]
      , requestSemaphore: Semaphore[F]
      , fromServer: Stream[F, IMAPData]
      , toServer: String => F[Unit]
    )(cmd: IMAPCommand)(implicit F: Async[F]): RequestResult[F] = {
      Stream.eval_(requestSemaphore.decrement) ++
      Stream.eval(idxRef.modify { _ + 1 } map { c => java.lang.Long.toHexString(c.now) }) flatMap { tag =>
        val commandLine = s"$tag ${cmd.asIMAPv4}\r\n"

        def unlock = Stream.eval(requestSemaphore.increment)

        Stream.eval_(toServer(commandLine)) ++
        fromServer.uncons1 flatMap {
          case None => unlock map { _ => Left("* BAD Connection with server terminated") }
          case Some((IMAPText(resp), tail)) =>
            if (resp.startsWith(tag)) {
              if (resp.drop(tag.size).trim.startsWith("OK")) unlock map { _ =>  Right(Stream.empty) } // no result ok was just received
              else unlock map { _ => Left(resp.drop(tag.size)) }  // failure
            } else {
              Stream(Right(
                Stream.emit[F, IMAPData](IMAPText(resp)) ++
                tail.takeThrough {
                  case IMAPText(l) => ! l.startsWith(tag)
                  case _  => true
                }.dropLastIf {
                  case IMAPText(l) => l.startsWith(tag)
                  case _ => false
                } onFinalize { requestSemaphore.increment }
              ))
            }

          case Some((IMAPBytes(_), _)) =>
            // Seems we are not in valid state. Streamed line cannot be the first line received
            // as the first line must contain {sz} macro.
            // so its safe to fail here w/o consuming the bytes.
            Stream.fail(new Throwable("Invalid client state initial response not received"))

        }

      }
    }


    /**
      * Runs supplied stream, and then will collect any output before `OK` is received.
      * @param stream   Stream that perform the request
      * @param f        A function to be evaluated on successfull response, and is fed with response from server,
      *                 concatenated w/o any  tags from server
      * @tparam F
      * @tparam A
      * @return
      */
    def shortContent[F[_], A](stream: RequestResult[F])(f: Seq[String] => F[A])(implicit F: Catchable[F]): F[Either[String, A]] = {
      stream.runLast flatMap {
        case None => F.pure(Left("Command failed to be processed, no output from server?"))
        case Some(Right(s)) =>
          (s through concatLines).runLog map { acc =>
            acc.map { s =>
              val line = s.dropWhile { c => c != '*'}
              if (line.headOption.contains('*')) line.tail
              else  line
            }
          } flatMap f map { a => Right(a) }
        case Some(Left(err)) => F.pure(Left(err))
      }
    }


    /** parses login response, returning any supported capabiliites **/
    def parseLogin[F[_]](lines: Seq[String])(implicit F: Monad[F]): F[Seq[String]] =
      parseCapability(lines)

    /** parse capability results **/
    def parseCapability[F[_]](lines: Seq[String])(implicit F: Monad[F]): F[Seq[String]] = {
      F.pure { lines.flatMap { l => l.trim.split("\\s").toSeq } }
    }

    /** parses result of LIST command **/
    def parseMailboxList[F[_]](lines: Seq[String])(implicit F: Monad[F]): F[Seq[IMAPMailbox]] = {
      F.pure { lines.flatMap(s => IMAPMailbox.fromListResult(s)) }
    }

    /** parses result of SELECT or EXAMINE commands **/
    def parseSelect[F[_]](lines: Seq[String])(implicit F: Monad[F]): F[IMAPMailboxStatus] =
      F.pure { IMAPMailboxStatus.parse(lines) }


    /** parses result of the search operation encoded as space delimeited ids of messages **/
    def parseSearchResult[F[_]](lines: Seq[String])(implicit F: Monad[F]): F[Seq[Long @@ MailUID]] =
      F.pure {
        lines.flatMap { l =>
          val trimmed = l.trim
          if (trimmed.startsWith("SEARCH"))
            trimmed.drop(6).split("\\s+").toSeq.flatMap { s => Try(tag[MailUID](java.lang.Long.parseLong(s))).toOption.toSeq }
          else Nil
        }
      }

    /** parses reult of FETCH xyz (BODYSTRUCTURE) request **/
    def parseBodyStructure[F[_]](lines: Seq[String])(implicit F: Effect[F]): F[Seq[EmailBodyPart]] = {
      val line = lines.mkString
      val indexStart = line.indexOf("(")
      if (indexStart < 0) F.fail(new Throwable("Could not find start of body structure."))
      else {
        IMAPBodyPartCodec.bodyStructure.decode(BitVector.view(line.drop(indexStart).getBytes)) match {
          case Attempt.Successful(result) => F.pure(EmailBodyPart.flatten(result.value))
          case Attempt.Failure(err) => F.fail(new Throwable(s"failed to decode BODYSTRUCTURE: $err ($lines)"))
        }
      }
    }





    /**
      * Causes to transform data from the fetch command to Binary content (Bytes)
      * given the specific contained in pa
      *
      * This will perfom decoding (i.e. base64 -> bytes) based on the supplied content data,
      * resulting in stream of bytes that contains binary representation of the content .
      *
      * @param contentIdx Index of content to take, all other content indexes will be ignored
      * @param contentKey Key of the content to retrieve, all other will be ignored
      * @param encoding   Encoding of the binary format
      * @tparam F
      * @return
      */
    def fetchBytesOf[F[_]](
      contentIdx: Int
      , contentKey: String
      , encoding: String
    ): Pipe[F, (Int, String, IMAPData),  Byte] = {

      val decoder: Pipe[F, Byte, Byte] = { s =>
        encoding.toUpperCase match {
          case "BASE64" => base64.decode[F](s)
          case "QUOTED-PRINTABLE" => quotedPrintable.decode[F](s)
          case "7BIT" | "8BIT" | "BINARY" => s
          case other => s.flatMap { _ => Stream.fail(new Throwable(s"Unsupported encoding: $other")) }
        }
      }

      _ through bytesOfSegment(contentIdx, contentKey) through decoder
    }

    /**
      * Causes to perform fetch command for the text of the email body.
      * Resulting data are available in text form (strings). The strings may not be whole lines or may contain multiple lines in each string.
      *
      * @param contentIdx   Index of content to take, all other content indexes will be ignored
      * @param contentKey   Key of the content to retrieve, all other will be ignored
      * @param encoding     Encoding of the data
      * @param charsetName  Name of the charset of the text. If empty, UTF-8 will be used instead
      */
    def fetchTextOf[F[_]](
      contentIdx: Int
      , contentKey: String
      , encoding: String
      , charsetName: Option[String]
    )(implicit F: Effect[F]): Pipe[F, (Int, String, IMAPData),  Char] = {

      val chs =
        charsetName
        .filter { Charset.isSupported }
        .map { Charset.forName }
        .getOrElse { StandardCharsets.UTF_8 }

      val decoder: Pipe[F, Byte, Char] = { s =>
        encoding.toUpperCase match {
          case "BASE64" => base64.decode[F](s) through charset.decode(chs)
          case "QUOTED-PRINTABLE" => quotedPrintable.decode[F](s) through charset.decode(chs)
          case "7BIT" | "8BIT" => s through charset.decode(chs)
          case other => s.flatMap { _ => Stream.fail(new Throwable(s"Unsupported encoding: $other")) }
        }
      }

      _ through bytesOfSegment(contentIdx, contentKey) through decoder
    }

    /**
      * For incoming computed data, this will convert to stream of bytes as received.
      * This will strip out any parts where idx and key is different from ones supplied.
      * Also this will strip out any non-binary content
      * @param idx    Index of result to look for
      * @param key    Key of the binary segment to receive
      */
    def bytesOfSegment[F[_]](idx: Int, key:String): Pipe[F, (Int, String, IMAPData), Byte] = {
      _ collect {
        case (`idx`, `key`, IMAPBytes(bs)) => bs
      } flatMap { bs =>
        Stream.chunk(ByteVectorChunk(bs))
      }
    }

    /**
      * From any stream of imap data this will strictly collect data for every response
      * in form of Map.
      *
      * Note that this strictly collects all data for every result denoted by idx.
      *
      */
    def fetchLog[F[_]]: Pipe[F, (Int, String, IMAPData), Map[String,  Vector[IMAPData]]] = {
      _.noneTerminate.mapAccumulate((0, Map.empty[String,  Vector[IMAPData]])) {
        case ((idx, acc), Some((idx0, k, v))) =>
          if (idx != idx0) ((idx0, Map(k -> Vector(v))), Stream.emit(acc))
          else {
            val acc0 = acc + (acc.get(k) map { vs => k -> (vs :+ v) } getOrElse (k -> Vector(v)))
            ((idx, acc0), Stream.empty)
          }

        case ((idx, acc), None) =>
          ((Int.MaxValue, Map.empty), Stream.emit(acc).filter(_.nonEmpty))
      } flatMap { _._2 }
    }


    /**
      * Creates email header from supplied of Map of Content.
      *
      * This requires UID content and BODY[HEADER] content to be present in map otherwise this will fail.
      */
    def mkEmailHeader[F[_]](
      headerCodec: Codec[EmailHeader]
    ): Pipe[F, Map[String,  Vector[IMAPData]], IMAPEmailHeader] = {
      def asString(data: Vector[IMAPData]): Either[String, String] = {
        val lines = data.collect { case IMAPText(s) => s }
        if (lines.size != data.size) Left(s"Invalid data for the string, only lines are expected: $data")
        else Right(lines.mkString)
      }

      def asBytes(data: Vector[IMAPData]): Either[String, ByteVector] = {
        val bytes = data.collect { case IMAPBytes(bs) => bs}
        if (bytes.size != data.size) Left(s"Invalid data for the bytes, only bytes are expected: $data")
        else Right(bytes.reduceOption(_ ++ _).getOrElse(ByteVector.empty))

      }

      def getUid(m: Map[String, Vector[IMAPData]]): Either[String, Long @@ MailUID] = {
        m.get("UID").map(Right(_)).getOrElse(Left("Missing UID key")).right flatMap { data =>
        asString(data).right flatMap { uidStr =>
          Try(java.lang.Long.parseLong(uidStr)).map { tag[MailUID](_)  } match {
            case Success(id) => Right(id)
            case Failure(err) => Left(s"Failed to parse int: $uidStr (${err.getMessage}")
          }
        }}
      }

      def getHeader(m: Map[String, Vector[IMAPData]]): Either[String, EmailHeader] = {
        m.get("BODY[HEADER]").map(Right(_)).getOrElse(Left("Missing BODY[HEADER] key")).right flatMap { data =>
        asBytes(data).right flatMap { hdrBytes =>
          headerCodec.decodeValue(hdrBytes.bits).toEither.left.map(_.messageWithContext)
        }}
      }

      _ map { m =>
        (
          for {
            uid <- getUid(m).right
            header <- getHeader(m).right
          } yield IMAPEmailHeader(header, uid)
        ).left.map(err => new Throwable(s"Invalid data for email: $err  ($m)"))
      } flatMap {
        case Right(h) => Stream.emit(h)
        case Left(err) => Stream.fail(err)
      }

    }


    // regex to indicate start of the record, drops the opening bracket
    private val startOfRecord = "^(?i)\\* \\d+ FETCH \\((.+)$".r

    // Indicates successfull result of the fetch, must be only non-result line in the fetch, then the fetch will terminate
    private val successFullFetch = "^(?i).+ OK .+$".r

    /**
      * From the supplied stream this will extract stream of raw content.
      *
      * This will fail, if result was not 'OK', with failure describing that exception.
      *
      * If the fetch will not receive any items, stream will be empty.
      *
      * As a result this will start to emit id of the result line (any line prefixed by *) then
      * FETCH result prefix (i.e. UID, BODY, ...) and content for that prefix. When multiple same
      * prefixes (Int, String) are emitted, they has tobe concatenated to form the result desired.
      *
      * @param result requestStream that produces result.
      * @tparam F
      * @return
      */
    def rawContent[F[_]](result: RequestResult[F]): Stream[F, (Int, String, IMAPData)] = {
      def collectBytes( recordIdx: Int, key: String, in: Stream[F, IMAPData]): Stream[F, (Int, String, IMAPData)] = {
        def go(curr: Stream[F, IMAPData]): Stream[F, (Int, String, IMAPData)] = {
          curr.uncons1 flatMap {
            case Some((d: IMAPBytes, tail)) =>
              Stream.emit((recordIdx, key, d)) ++ go(tail)

            case Some((d: IMAPText, tail)) =>
              findEntry(recordIdx, Stream.emit(d) ++ tail)

            case None =>
              Stream.fail(new Throwable(s"Expected end of bytes for record: $recordIdx, key: $key, but EOF reached"))
          }
        }

        in.uncons1 flatMap {
          case Some((d@IMAPBytes(bs), tail)) =>
            Stream.emit((recordIdx, key, d)) ++ go(tail)

          case Some((IMAPText(l), _)) =>
            Stream.fail(new Throwable(s"Expected bytes for record: $recordIdx, key: $key, but got $l"))

          case None =>
            Stream.fail(new Throwable(s"Expected bytes for record: $recordIdx, key: $key, but EOF reached"))
        }
      }


      // finds entry in fetch response. Note that this will advance to find record once line starting with `*` is found.
      def findEntry(recordIdx: Int, in: Stream[F, IMAPData]): Stream[F, (Int, String, IMAPData)] = {
        in.uncons1 flatMap {
          case Some((d@IMAPText(l), tail)) =>
            if (l.startsWith("*")) findRecord(recordIdx + 1, Stream.emit(d) ++ tail)
            else if (l.trim.startsWith(")")) findRecord(recordIdx + 1, tail) // just advance to next record, ignore line content
            else {
              // key is always terminated with space. lets try to find it
              val trimmed = l.trim
              if (trimmed.isEmpty)  Stream.fail(new Throwable(s"Expected key of FETCH response, but got empty line"))
              else {
                val (key, valueCandidate) = {
                  val keyIdx = trimmed.indexOf(' ')
                  if (keyIdx < 0) (trimmed, "")
                  else (trimmed.take(keyIdx), trimmed.drop(keyIdx).trim)
                }

                def output(offset: Int, sz: Int) = { // end is > 0
                  val value = valueCandidate.slice(offset, sz)
                  Stream.emit((recordIdx, key, IMAPText(value))) ++
                  findEntry(recordIdx, Stream.emit(IMAPText(valueCandidate.drop(offset + sz + 1))) ++ tail)
                }

                if (valueCandidate.startsWith("(")) {
                  // take everything up to the ending bracket, fail if string does not have one
                  // value candidates shall not have nested brackets here
                  // todo: add more principled parsing based on the type of the key
                  val end = valueCandidate.indexOf(')')
                  if (end < 0) Stream.fail(new Throwable(s"Expected value for $key in brackets, but got $valueCandidate"))
                  else output(1, end - 1)
                } else {
                  // take everything till next space, or switch to bytes mode if no entry
                  val end = valueCandidate.indexOf(' ')
                  if (end <= 0) Stream.emit((recordIdx, key, IMAPBytes(ByteVector.empty))) ++ collectBytes(recordIdx, key, tail)
                  else output(0, end)
                }
              }
            }

          case Some((IMAPBytes(bv), tail)) =>
            Stream.fail(new Throwable(s"Got bytes, when key of record was expected: $bv"))

          case None =>
            Stream.empty
        }
      }


      // Tries to find start of imap record, essentialy line starting with `* <tag> FETCH (` pattern
      def findRecord(recordIdx: Int, in: Stream[F, IMAPData]): Stream[F, (Int, String, IMAPData)] = {
        in.uncons1 flatMap {
          case Some((IMAPText(l), tail)) =>
            startOfRecord.findFirstMatchIn(l).flatMap(m => Option(m.group(1))) match {
            case Some(start) =>
              findEntry(recordIdx, Stream.emit(IMAPText(start)) ++ tail)
            case None =>
              if (successFullFetch.findFirstMatchIn(l).nonEmpty) Stream.empty
              else Stream.fail(new Throwable(s"Expected start of record at $recordIdx, but got $l"))
          }

          case Some((IMAPBytes(bv), _)) =>
            Stream.fail(new Throwable(s"Got bytes, when start of record was expected: $bv"))

          case None =>
            Stream.empty
        }
      }

      result.flatMap {
        case Right(stream) =>
          findRecord(0, stream).scope
        case Left(err) =>
          Stream.fail(new Throwable(s"Failed to perform the command: $err"))
      }

    }


    /** from the stream of lines build string of string where {sz} expanded segments are returned as ASCII string (not individual lines)**/
    def concatLines[F[_]]: Pipe[F, IMAPData, String] = { _ map {
      case IMAPText(l) => l
      case IMAPBytes(bv) => new String(bv.toArray)
    }}

    // match for line sz parameters to extract lines content and size of bytes coming next.
    private val lineSzMatch: Regex = "^(.*)\\{(\\d+)\\}$".r

    /**
      * Separates incoming stream based on CRLF (\r\n) separator. When line contains as last characters
      * chunk  macro ({sz}\r\n) then, such macro is expanded to IMAPBytes(bytes)  that
      * allows to handle these contents differently (i.e. body in fetch response). There may by multiple IMAPBytes received,
      * up to the size `sz` specified in macro.
      *
      * Please note this will decode incoming bytes (for lines) with ASCII character set hence the content of the IMAP responses
      * is based on 7 bit ASCII subset. The bytes in DataChunk are leaved as received, allowing to apply
      * various encodings.
      *
      */
    def lines[F[_]]: Pipe[F, Byte, IMAPData] = {
      val crlf = ByteVector.view("\r\n".getBytes)

      def collectChunk(sz: Int, tail: Stream[F, Byte]): Stream[F, IMAPData] = {
        tail.uncons.flatMap {
          case Some((chunk, tail)) =>
            val bv = ByteVectorChunk.asByteVector(chunk)
            if (bv.size < sz) Stream.emit(IMAPBytes(bv)) ++ collectChunk(sz - bv.size.toInt, tail)
            else {
              val out = bv.take(sz)
              Stream.emit(IMAPBytes(out)) ++ collectLines(ByteVector.empty, Stream.chunk(chunk.drop(sz)) ++ tail)
            }

          case None =>
            Stream.empty
        }
      }

      def collectLines(buff: ByteVector, tail: Stream[F, Byte]):  Stream[F, IMAPData] = {
        tail.uncons.flatMap {
          case Some((chunk, tail)) =>
            val nb = buff ++ ByteVectorChunk.asByteVector(chunk)
            val lineIdx = nb.indexOfSlice(crlf)
            if (lineIdx < 0) collectLines(nb, tail)
            else {
              val (bh, bt) = nb.splitAt(lineIdx)
              bh.decodeAscii match {
                case Right(s) =>
                  if (s.endsWith("}")) {
                    lineSzMatch.findFirstMatchIn(s) match {
                      case Some(m) =>
                        val line = m.group(1)
                        val num = m.group(2)
                        if (line == null || num == null) Stream.fail(new Throwable(s"Expected line and num match, got: $line, $num from $s"))
                        else {
                          Try(num.toInt) match {
                            case Success(sz) =>
                              Stream.emit(IMAPText(line)) ++
                                collectChunk(sz, Stream.chunk(ByteVectorChunk(bt.drop(2))) ++ tail)

                            case Failure(err) =>
                              Stream.fail(err)
                          }
                        }
                      case None => Stream.fail(new Throwable(s"Expected {sz} macro at end of line got : $s"))
                    }
                  } else {
                    Stream.emit(IMAPText(s)) ++
                    collectLines(ByteVector.empty, Stream.chunk(ByteVectorChunk(bt.drop(2))) ++ tail)
                  }

                case Left(err) => Stream.fail(err)
              }
            }

          case None => Stream.empty  // ignore last line not terminated by crlf
        }
      }

      s => collectLines(ByteVector.empty, s).scope
    }
  }
}
