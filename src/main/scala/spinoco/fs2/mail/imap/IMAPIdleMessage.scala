package spinoco.fs2.mail.imap

import shapeless.tag.@@


object IMAPIdleMessage {

  /**
    * Notification about new email with a given id.
    * @param uid  The id of the new email.
    */
  case class Exists(uid: Long @@ MailUID) extends IMAPIdleMessage

  /**
    * Notifies a given email being deleted on the server.
    * @param uid The id of the email that was deleted.
    */
  case class Expunge(uid: Long @@ MailUID) extends IMAPIdleMessage

  /**
    * A message with a type that we are not expecting appeared.
    * @param uid  The id of the email this message concerns.
    */
  case class CustomMessage(name: String, uid: Long @@ MailUID) extends IMAPIdleMessage

  /**
    * Build the proper message from the provided message type.
    * @param messageType  The type of the message to be constructed.
    * @param emailUid     The id of the message which this message concerns.
    * @return
    */
  def parse(messageType: String, emailUid: Long @@ MailUID): IMAPIdleMessage = {
    messageType match {
      case "EXISTS" => Exists(emailUid)
      case "EXPUNGE" => Expunge(emailUid)
      case other => CustomMessage(other, emailUid)
    }
  }
}

/** Messages that the server pushes to the client while the client requested IDLE **/
sealed trait IMAPIdleMessage
