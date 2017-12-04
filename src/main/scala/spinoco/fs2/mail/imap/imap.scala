package spinoco.fs2.mail

package object imap {

  /** tagged trait fro mailbox names **/
  sealed trait MailboxName

  /** Unique email identifier in given mailbox **/
  sealed trait MailUID

  /** result of IMAP Operation **/
  type IMAPResult[A] = Either[String, A]

}
