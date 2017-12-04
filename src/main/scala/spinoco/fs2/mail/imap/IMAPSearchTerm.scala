package spinoco.fs2.mail.imap

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import spinoco.protocol.mail.header.DestinationType

object IMAPSearchTerm {

  /**  All messages in the mailbox **/
  val ALL: IMAPSearchTerm = IMAPSearchTerm("ALL")

  val ANSWERED: IMAPSearchTerm = WithFlag("\\Answered")
  val DELETED: IMAPSearchTerm = WithFlag("\\Deleted")
  val DRAFT: IMAPSearchTerm = WithFlag("\\Draft")
  val FLAGGED: IMAPSearchTerm = WithFlag("\\Flagged")

  val RECENT: IMAPSearchTerm = WithFlag("\\Recent")
  val SEEN: IMAPSearchTerm = WithFlag("\\Seen")

  /** Messages with the flag set.**/
  def WithFlag(flag: String): IMAPSearchTerm = flag match {
    case "\\Answered" => IMAPSearchTerm("ANSWERED")
    case "\\Deleted" => IMAPSearchTerm("DELETED")
    case "\\Draft" => IMAPSearchTerm("DRAFT")
    case "\\Flagged" => IMAPSearchTerm("FLAGGED")
    case "\\Recent" => IMAPSearchTerm("RECENT")
    case "\\Seen" => IMAPSearchTerm("SEEN")
    case other => IMAPSearchTerm(s"KEYWORD $other")
  }


  val UNANSWERED: IMAPSearchTerm = WithoutFlag("\\Answered")
  val UNDELETED: IMAPSearchTerm = WithoutFlag("\\Deleted")
  val UNDRAFT: IMAPSearchTerm = WithoutFlag("\\Draft")
  val UNFLAGGED: IMAPSearchTerm = WithoutFlag("\\Flagged")
  val UNRECENT: IMAPSearchTerm = WithoutFlag("\\Recent")
  val UNSEEN: IMAPSearchTerm = WithoutFlag("\\Seen")

  def WithoutFlag(flag: String): IMAPSearchTerm = flag match {
    case "\\Answered" => IMAPSearchTerm("UNANSWERED")
    case "\\Deleted" => IMAPSearchTerm("UNDELETED")
    case "\\Draft" => IMAPSearchTerm("UNDRAFT")
    case "\\Flagged" => IMAPSearchTerm("UNFLAGGED")
    case "\\Recent" => IMAPSearchTerm("UNRECENT")
    case "\\Seen" => IMAPSearchTerm("UNSEEN")
    case other => IMAPSearchTerm(s"UNKEYWORD $other")
  }

  def To(str: String): IMAPSearchTerm = Destination(DestinationType.To, str)
  def Cc(str: String): IMAPSearchTerm = Destination(DestinationType.Cc, str)
  def Bcc(str: String): IMAPSearchTerm = Destination(DestinationType.Bcc, str)

  /**
    * Messages that contain the specified string in the envelope
    * structure's `tpe` field.
    * @param tpe  Type of the destination field
    * @param str  String to search for
    */
  def Destination(tpe: DestinationType.Value, str: String): IMAPSearchTerm =
    IMAPSearchTerm(s"""${tpe.toString.toUpperCase} "$str"""")


  /**
    * Messages that contain the specified string in the envelope
    * structure's  From field.
    * @param str  String to search for
    */
  def From(str: String): IMAPSearchTerm = IMAPSearchTerm(s"""FROM "${str}"""")

  private val formatter  =  DateTimeFormatter.ofPattern("dd-MMM-yyyy")

  /**
    * Messages whose internal date (disregarding time and timezone)
    * is earlier than the specified date.
    * @param date Date before
    */
  def Before(date: LocalDate): IMAPSearchTerm = IMAPSearchTerm(s"BEFORE ${formatter.format(date)}")

  /**
    * Messages whose internal date (disregarding time and timezone)
    * is within the specified date.
    */
  def On(date: LocalDate): IMAPSearchTerm = IMAPSearchTerm(s"ON ${formatter.format(date)}")

  /**
    * Messages whose internal date (disregarding time and timezone)
    *is within or later than the specified date.
    */
  def Since(date: LocalDate): IMAPSearchTerm = IMAPSearchTerm(s"SINCE ${formatter.format(date)}")

  /**
    * Messages whose [RFC-2822] Date: header (disregarding time and
    * timezone) is earlier than the specified date.
    * @param date
    */
  def SentBefore(date: LocalDate): IMAPSearchTerm = IMAPSearchTerm(s"SENTBEFORE ${formatter.format(date)}")


  /**
    * Messages whose [RFC-2822] Date: header (disregarding time and
    * timezone) is within the specified date.
    * @param date
    */
  def SentOn(date: LocalDate): IMAPSearchTerm = IMAPSearchTerm(s"SENTON ${formatter.format(date)}")

  /**
    * Messages whose [RFC-2822] Date: header (disregarding time and
    * timezone) is within or later than the specified date.
    * @param date
    */
  def SentSince(date: LocalDate): IMAPSearchTerm = IMAPSearchTerm(s"SENTSINCE ${formatter.format(date)}")

  /**
    * Messages that contain the specified string in the body of the
    * message.
    */
  def Body(s: String): IMAPSearchTerm = IMAPSearchTerm(s"""BODY "$s"""")

  /**
    * Messages that have a header with the specified field-name (as
    * defined in [RFC-2822]) and that contains the specified string
    * in the text of the header (what comes after the colon).  If the
    * string to search is zero-length, this matches all messages that
    * have a header line with the specified field-name regardless of
    * the contents.
    * @param name   Header name
    * @param value  value to search for
    */
  def Header(name: String, value: String): IMAPSearchTerm = IMAPSearchTerm(s"""HEADER $name "$value"""")


  /**
    * Messages with an [RFC-2822] size larger than the specified
    * number of octets.
    * @param size Size in octets (bytes)
    */
  def Larger(size: Long): IMAPSearchTerm = IMAPSearchTerm(s"LARGER $size")

  /**
    * Messages with an [RFC-2822] size smaller than the specified
    * number of octets.
    * @param size Size in octets (bytes)
    */
  def Smaller(size: Long): IMAPSearchTerm = IMAPSearchTerm(s"SMALLER $size")

  /**
    * Messages that do not match the specified search term.
    */
  def Not(term: IMAPSearchTerm*): IMAPSearchTerm = IMAPSearchTerm(s"NOT (${term.map(_.term).mkString(" ")})")

  /**
    * Messages that have the \Recent flag set but not the \Seen flag.
    * This is functionally equivalent to "(RECENT UNSEEN)".
    */
  val NEW: IMAPSearchTerm = IMAPSearchTerm("NEW")

  /**
    * Messages that do not have the \Recent flag set.  This is
    * functionally equivalent to "NOT RECENT" (as opposed to "NOT
    * NEW").
    */
  val OLD: IMAPSearchTerm = IMAPSearchTerm("OLD")

  /**
    * Messages that do not match the specified search term.
    */
  def Subject(s: String): IMAPSearchTerm = IMAPSearchTerm(s"""SUBJECT "$s"""")


  /**
    * Messages that contain the specified string in the header or
    * body of the message.
    */
  def Text(s: String): IMAPSearchTerm = IMAPSearchTerm(s"""TEXT "$s"""")

  /**
    * Messages with unique identifiers corresponding to the specified
    * unique identifier set.
    * @param uids individual uids
    * @return
    */
  def UID(uids: Long*): IMAPSearchTerm =
    IMAPSearchTerm(s"UID ${uids.mkString(",")}")

  /**
    * Messages with unique identifiers corresponding to the specified
    * unique identifier set.
    * @param ranges individual uid ranges
    * @return
    */
  def UIDRange(ranges: Range*): IMAPSearchTerm =
    IMAPSearchTerm(s"UID ${ranges.flatMap { r => r.headOption flatMap { from => r.lastOption map { to => s"$from:$to" } } }.mkString(",") }")
}


case class IMAPSearchTerm(term: String) { self =>
  /** this and others terms must match **/
  def and(other: IMAPSearchTerm*): IMAPSearchTerm = IMAPSearchTerm(s"$self $other")

  /** this or others terms must match **/
  def or(other: IMAPSearchTerm*): IMAPSearchTerm = IMAPSearchTerm(s"OR ($self $other)")
}
