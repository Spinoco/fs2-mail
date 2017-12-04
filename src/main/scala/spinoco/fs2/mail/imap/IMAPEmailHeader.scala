package spinoco.fs2.mail.imap


import shapeless.tag.@@
import spinoco.protocol.mail.EmailHeader

/**
  * Arbitrary IMAP Email Header, according to RFC 5322 (https://tools.ietf.org/html/rfc5322)
  *
  * @param header   Header of this email
  * @param uid      Uid of the email.
  */
case class IMAPEmailHeader(
  header: EmailHeader
  , uid: Long @@ MailUID
)

