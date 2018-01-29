package spinoco.fs2.mail.imap

import fs2.Stream
import fs2.util.Async
import shapeless.tag
import spinoco.fs2.mail.imap.IMAPClient.impl.{IMAPBytes, IMAPData, IMAPText}

/**
  * The context of an idling IMAP server connection that is ready to receive notifications from server.
  */
trait IMAPIdleContext[F[_]] {

  /**
    * The IMAP IDLE events that are being produced as result of calling the IDLE command.
    *
    * This call can be made only once. As this is not backed by a [[fs2.async.mutable.Topic]], thus
    * the data of each call of "events" would be different.
    *
    * Thus this will fail any subsequent calls to events.
    */
  def events: Stream[F, IMAPIdleMessage]

  /**
    * Closes the IDLE connection with server.
    *
    * This returns as soon as it writes to the socket.
    * To get the end of the idle output await on the events stream, which will
    * end as soon as the server stops feeding the data to the client.
    */
  def done: F[Unit]

}

object IMAPIdleContext {

  /**
    * Creates a IDLE context that has events method guarded against multiple calls to it.
    *
    * @param incoming       The data from server as result of IDLE command.
    * @param writeToServer  A method to write to server.
    */
  def mk[F[_]](
    incoming: Stream[F, IMAPData]
    , writeToServer: String => F[Unit]
  )(
    implicit F: Async[F]
  ): F[IMAPIdleContext[F]] = {
    F.map(F.refOf(false)) { startedRead =>
      new IMAPIdleContext[F] {
        def done: F[Unit] = writeToServer("DONE")

        def events: Stream[F, IMAPIdleMessage] = {
          Stream.eval(startedRead.modify(_ => true)).flatMap { change =>
            if (change.previous) Stream.fail(new Throwable("Cannot try to subscribe to idle events for the second time"))
            else impl.makeIdleEvents(incoming)
          }
        }
      }

    }
  }


  object impl {

    /**
      * Parses out the IDLE IMAP events from preprocessed server data.
      *
      * @param in The data from the server already split by lines.
      */
    def makeIdleEvents[F[_]](in: Stream[F, IMAPData]): Stream[F, IMAPIdleMessage] = {
      in.flatMap{
        case _: IMAPBytes => Stream.empty //IDLE cannot give bytes, maybe fail here?
        case IMAPText(line) =>
          val trimmed = line.trim
          if (trimmed.startsWith("+ idling")) Stream.empty //Notified about now idling, server accepted we can carry on
          else if (trimmed.startsWith("*")) {
            val dropped = trimmed.drop(1).trim
            val splitIdx = dropped.indexOf(" ")
            if (splitIdx < 0) Stream.fail(new Throwable(s"Could not find split between email id and command while parsing IMAP IDLE line: $line"))
            else {
              val (email, messageType) = dropped.splitAt(splitIdx)
              val emailUid = tag[MailUID](email.toLong)
              Stream.emit(IMAPIdleMessage.parse(messageType.trim, emailUid))

            }
          } else Stream.fail(new Throwable(s"Line starts wih an unknown character while parsing IMAP IDLE line: $line"))
      }
    }
  }
}