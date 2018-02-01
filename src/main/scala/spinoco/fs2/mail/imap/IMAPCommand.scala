package spinoco.fs2.mail.imap

import shapeless.tag.@@

import scala.collection.immutable.NumericRange


object IMAPCommand {

  final case class LoginPlainText(user: String, pass: String) extends IMAPCommand {
    def asIMAPv4: String = s"LOGIN $user $pass"
  }

  final case object Logout extends IMAPCommand {
    def asIMAPv4: String = "LOGOUT"
  }

  final case object Capability extends IMAPCommand {
    def asIMAPv4: String = "CAPABILITY"
  }

  final case class ListMailbox(reference:String, wildcard: String) extends IMAPCommand {
    def asIMAPv4: String = s"""LIST "$reference" "$wildcard""""
  }

  final case class Examine(name: String @@ MailboxName) extends IMAPCommand {
    def asIMAPv4: String = s"""EXAMINE "$name""""
  }


  final case class Select(name: String @@ MailboxName) extends IMAPCommand {
    def asIMAPv4: String = s"""SELECT "$name""""
  }

  final case class Search(charset: Option[String], term: IMAPSearchTerm) extends IMAPCommand {
    def asIMAPv4: String = charset match {
      case None => s"SEARCH ${term.term}"
      case Some(chset) => s"SEARCH CHARSET $chset ${term.term}"
    }
  }

  final case class Fetch(range: NumericRange[Long], content: Seq[IMAPFetchContent]) extends IMAPCommand {
    def asIMAPv4: String = {
      val contentString = s"(${content.map(_.content).mkString(" ")})"
      if (range.start == range.end) s"FETCH ${range.start} $contentString"
      else s"FETCH ${range.start}:${range.end} $contentString"
    }
  }

  final case object Idle extends IMAPCommand {
    def asIMAPv4: String = "IDLE"
  }

}



sealed trait IMAPCommand {
  def asIMAPv4: String
}

