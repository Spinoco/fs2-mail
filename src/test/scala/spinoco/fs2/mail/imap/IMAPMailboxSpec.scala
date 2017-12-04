package spinoco.fs2.mail.imap

import org.scalacheck.Properties
import org.scalacheck.Prop._
import shapeless.tag

object IMAPMailboxSpec extends Properties("IMAPMailbox"){

  val encoded = List(
    """LIST (\HasNoChildren) "/" "INBOX" """
    , """ LIST (\HasNoChildren) "/" "Priorita" """
    , """ LIST (\HasNoChildren) "/" "Reagovat" """
    , """ LIST (\HasNoChildren) "/" "R&AW8-zn&AOk-" """
    , """ LIST (\HasChildren \Noselect) "/" "[Gmail]" """
    , """ LIST (\HasNoChildren \Important) "/" "[Gmail]/D&AW8-le&AX4-it&AOk-" """
    , """ LIST (\Drafts \HasNoChildren) "/" "[Gmail]/Koncepty" """
    , """ LIST (\HasNoChildren \Trash) "/" "[Gmail]/Ko&AWE-" """
    , """ LIST (\HasNoChildren \Sent) "/" "[Gmail]/Odeslan&AOE- po&AWE-ta" """
    , """ LIST (\Flagged \HasNoChildren) "/" "[Gmail]/S hv&ARs-zdi&AQ0-kou" """
    , """ LIST (\HasNoChildren \Junk) "/" "[Gmail]/Spam" """
    , """ LIST (\All \HasNoChildren) "/" "[Gmail]/V&AWE-echny zpr&AOE-vy" """
    , """ LIST "/" "[Gmail]/V&AWE-echny zpr&AOE-vy" """
  )

  property("decode-mailbox") = protect {
    val sz = 1

    encoded.flatMap(s => IMAPMailbox.fromListResult(s).toSeq) ?= List(
      IMAPMailbox("/", tag[MailboxName]("INBOX"), List("\\HasNoChildren"))
      , IMAPMailbox("/", tag[MailboxName]("Priorita"), List("\\HasNoChildren"))
      , IMAPMailbox("/", tag[MailboxName]("Reagovat"), List("\\HasNoChildren"))
      , IMAPMailbox("/", tag[MailboxName]("Různé"), List("\\HasNoChildren"))
      , IMAPMailbox("/", tag[MailboxName]("[Gmail]"), List("\\HasChildren", "\\Noselect"))
      , IMAPMailbox("/", tag[MailboxName]("[Gmail]/Důležité"), List("\\HasNoChildren", "\\Important"))
      , IMAPMailbox("/", tag[MailboxName]("[Gmail]/Koncepty"), List("\\Drafts", "\\HasNoChildren"))
      , IMAPMailbox("/", tag[MailboxName]("[Gmail]/Koš"), List("\\HasNoChildren", "\\Trash"))
      , IMAPMailbox("/", tag[MailboxName]("[Gmail]/Odeslaná pošta"), List("\\HasNoChildren", "\\Sent"))
      , IMAPMailbox("/", tag[MailboxName]("[Gmail]/S hvězdičkou"), List("\\Flagged", "\\HasNoChildren"))
      , IMAPMailbox("/", tag[MailboxName]("[Gmail]/Spam"), List("\\HasNoChildren", "\\Junk"))
      , IMAPMailbox("/", tag[MailboxName]("[Gmail]/Všechny zprávy"), List("\\All", "\\HasNoChildren"))
      , IMAPMailbox("/", tag[MailboxName]("[Gmail]/Všechny zprávy"), Nil)
    )

  }


}
