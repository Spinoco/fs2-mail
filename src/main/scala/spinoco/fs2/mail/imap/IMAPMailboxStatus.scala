package spinoco.fs2.mail.imap

import shapeless.tag.@@

import scala.util.Try
import scala.util.matching.Regex
import shapeless.{Lens, lens, tag}

import scala.annotation.tailrec

/**
  * INidctaes status of Mailbox. This is received as result to SELECT or EXAMINE ldap commands
  *
  * @param flags              Defined flags in the mailbox
  * @param permanentFlags     A list of message flags that the client can change
  *                           permanently.  If this is missing, the client should
  *                           assume that all flags can be changed permanently
  * @param exists             The number of messages in the mailbox
  * @param recent             The number of messages with the \Recent flag set.
  * @param unseen             The message sequence number of the first unseen
  *                           message in the mailbox.  If this is missing, the
  *                           client can not make any assumptions about the first
  *                           unseen message in the mailbox, and needs to issue a
  *                           SEARCH command if it wants to find it.
  * @param uidNext            The next unique identifier value.
  * @param uidValidity        The unique identifier validity value.  If this is
  *                           missing, the server does not support unique
  *                           identifiers.
  */
case class IMAPMailboxStatus(
  flags: Seq[String]
  , permanentFlags: Seq[String]
  , exists: Int
  , recent: Int
  , unseen: Option[Int]
  , uidNext: Long @@ MailUID
  , uidValidity: Option[Long]
)


object  IMAPMailboxStatus {


  /**
    * Parses input lines to produce the status. Invalid lines are ignored.
    */
  val parse: Seq[String] => IMAPMailboxStatus = {

    val patternExists = """\s*(\d+)\s+EXISTS\s*""".r
    val patternRecent = """\s*(\d+)\s+RECENT\s*""".r
    val patternUnseen = """\s*OK\s+\[\s*UNSEEN\s+(\d+)\s*\].*""".r
    val patternFlags = """\s*FLAGS\s+\((.*)\)\s*""".r
    val patternPermanentFlags = """\s*OK\s+\[\s*PERMANENTFLAGS\s+\((.*)\)\s*\].*""".r
    val patternUidNext = """\s*OK\s+\[\s*UIDNEXT\s+(\d+)\s*\].*""".r
    val patternUidValidity = """\s*OK\s+\[\s*UIDVALIDITY\s+(\d+)\s*\].*""".r

    val lensExists = lens[IMAPMailboxStatus] >> 'exists
    val lensRecent = lens[IMAPMailboxStatus] >> 'recent
    val lensUnseen = lens[IMAPMailboxStatus] >> 'unseen
    val lensFlags = lens[IMAPMailboxStatus] >> 'flags
    val lensPermanentFlags = lens[IMAPMailboxStatus] >> 'permanentFlags
    val lensUidNext =  new Lens[IMAPMailboxStatus, Long @@ MailUID] { // somehow the macro does not like the  @@ tagged fields ?
      def get(s: IMAPMailboxStatus) = s.uidNext
      def set(s: IMAPMailboxStatus)(a: @@[Long, MailUID]) = s.copy(uidNext = a)
    }
    val lensUidValidity = lens[IMAPMailboxStatus] >> 'uidValidity

    /*
     * Used to construct patterns, that are used to match lines and then modify status of mailbox
     */
    def applyPattern[A](
      regex: Regex
      , lens: Lens[IMAPMailboxStatus, A]
    )(f: String => Option[A]): (IMAPMailboxStatus, Seq[String]) => (IMAPMailboxStatus, Seq[String]) = {
      val f0 =  regex.findFirstMatchIn _ andThen { _.flatMap { m => Option(m.group(1)) } flatMap f }

      (s, lines) =>

      @tailrec
      def go(rem: Seq[String], acc: Vector[String]): (IMAPMailboxStatus, Seq[String]) = {
        rem.headOption match {
          case Some(l) => f0(l) match {
            case Some(a) => (lens.set(s)(a), acc ++ rem.tail)
            case None => go(rem.tail, acc :+ l)
          }
          case None => (s, acc)
        }
      }

      go(lines, Vector.empty)
    }

    val applyExists = applyPattern(patternExists, lensExists) { s => Try(Integer.parseInt(s)).toOption } tupled
    val applyRecent = applyPattern(patternRecent, lensRecent) { s => Try(Integer.parseInt(s)).toOption } tupled
    val applyUnseen = applyPattern(patternUnseen, lensUnseen) { s => Try(Integer.parseInt(s)).toOption.map(Some(_)) } tupled
    val applyFlags = applyPattern(patternFlags, lensFlags) { s => Some(s.split("\\s")) } tupled
    val applyPermanentFlags = applyPattern(patternPermanentFlags, lensPermanentFlags) { s => Some(s.split("\\s")) } tupled
    val applyUidNext = applyPattern(patternUidNext, lensUidNext) { s => Try(java.lang.Long.parseLong(s)).toOption map { l => tag[MailUID](l)} } tupled
    val applyUidValidity = applyPattern(patternUidValidity, lensUidValidity) { s => Try(java.lang.Long.parseLong(s)).toOption.map(Some(_)) } tupled

    val parser =
    applyExists andThen
      applyRecent andThen
      applyUnseen andThen
      applyFlags andThen
      applyPermanentFlags andThen
      applyUidNext andThen
      applyUidValidity

    lines =>

      parser((
        IMAPMailboxStatus(Nil, Nil, 0, 0, None, tag[MailUID](0l), None)
        , lines
      ))._1

  }

}
