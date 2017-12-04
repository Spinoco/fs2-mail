package spinoco.fs2.mail.imap

import shapeless.tag
import shapeless.tag.@@
import spinoco.fs2.mail.encoding

case class IMAPMailbox(
  reference: String
  , mailbox: String @@ MailboxName
  , attributes: List[String]
)

object IMAPMailbox {

  // regex to expand the LIST response
  private val mailboxRegex = """\s*LIST\s+(\(.+\)\s+)?"(.*)"\s+"(.*)"\s*""".r

  /**
    * Eventually parses imapa mailoby yielding to None, if mailbox couldn't get parsed.
    *
    * The supported format is :
    *
    * LIST (attributes) "reference" "name"
    *
    * Note that `attributes` is optional, whereas `reference` and `name` are required.
    * Also note, that even when quoted, both the reference and name may have encoded characters
    *
    *
    */
  def fromListResult(result: String): Option[IMAPMailbox] = {

    // parse attributes stripping any leading slash
    def parseAttributes(s: String): List[String] = {
      s.split("\\s") toList
    }

    // parses referenceor name enclosed in double quotes.
    def parseRefOrName(s: String): Option[String] = {
      encoding.decodeIMAPUtf7(s).toOption
    }


    mailboxRegex.findFirstMatchIn(result) flatMap { m =>
      val attrs = if (m.group(1) == null) Nil else parseAttributes(m.group(1).trim.tail.init)
      val ref = m.group(2)
      val name = m.group(3)

      parseRefOrName(ref) flatMap { reference =>
      parseRefOrName(name) map { name =>
        IMAPMailbox(reference, tag[MailboxName](name), attrs)
      }}
    }


  }

}
