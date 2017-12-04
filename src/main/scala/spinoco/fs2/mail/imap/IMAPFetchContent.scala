package spinoco.fs2.mail.imap

case class IMAPFetchContent(content: String)

object IMAPFetchContent {

  /** Macro equivalent to: (FLAGS INTERNALDATE RFC822.SIZE ENVELOPE) **/
  val ALL : IMAPFetchContent = IMAPFetchContent("ALL")

  /** Macro equivalent to: (FLAGS INTERNALDATE RFC822.SIZE ENVELOPE) **/
  val FAST : IMAPFetchContent = IMAPFetchContent("FAST")

  /** Macro equivalent to: (FLAGS INTERNALDATE RFC822.SIZE ENVELOPE) **/
  val FULL : IMAPFetchContent = IMAPFetchContent("FULL")

  /** Macro equivalent to: (FLAGS INTERNALDATE RFC822.SIZE ENVELOPE) **/
  val BODY : IMAPFetchContent = IMAPFetchContent("BODY")

  /**
    * The text of a particular body section.  The section
    * specification is a set of zero or more part specifiers
    */
  def Body(sections: BodySection*): IMAPFetchContent =
    IMAPFetchContent(s"BODY[${sections map { _.section } mkString "," }]")

  /**
    * The text of a particular body section.  The section
    * specification is a set of zero or more part specifiers
    *
    * @param range      particular range of email to fetch
    * @param sections   Sections to fetch
    */
  def BodyRange(range: Range, sections: BodySection*): IMAPFetchContent =
    IMAPFetchContent(s"BODY[${sections map { _.section } mkString "," }]<${range.start}.${range.end}>")


  /**
    * An alternate form of BODY[<section>] that does not implicitly
    * set the \Seen flag.
    */
  def BodyPeek(sections: BodySection*): IMAPFetchContent =
    IMAPFetchContent(s"BODY.PEEK[${sections map { _.section } mkString "," }]")

  /**
    * An alternate form of BODY[<section>] that does not implicitly
    * set the \Seen flag.
    *
    * @param range      particular range of email to fetch
    * @param sections   Sections to fetch
    */
  def BodyPeekRange(range: Range, sections: BodySection*): IMAPFetchContent =
    IMAPFetchContent(s"BODY.PEEK[${sections map { _.section } mkString "," }]<${range.start}.${range.end}>")

  /**
    * The [MIME-IMB] body structure of the message.  This is computed
    * by the server by parsing the [MIME-IMB] header fields in the
    * [RFC-2822] header and [MIME-IMB] headers.
    */
  val BODYSTRUCTURE : IMAPFetchContent = IMAPFetchContent("BODYSTRUCTURE")

  /**
    * The envelope structure of the message.  This is computed by the
    * server by parsing the [RFC-2822] header into the component
    * parts, defaulting various fields as necessary.
    */
  val ENVELOPE : IMAPFetchContent = IMAPFetchContent("ENVELOPE")

  /**
    * The flags that are set for this message.
    */
  val FLAGS : IMAPFetchContent = IMAPFetchContent("FLAGS")

  /**
    * The internal date of the message.
    */
  val INTERNALDATE : IMAPFetchContent = IMAPFetchContent("INTERNALDATE")

  /**
    *  Functionally equivalent to BODY[], differing in the syntax of
    *  the resulting untagged FETCH data (RFC822 is returned).
    */
  val RFC822 : IMAPFetchContent = IMAPFetchContent("RFC822")

  /**
    *  Functionally equivalent to BODY.PEEK[HEADER], differing in the
    *  syntax of the resulting untagged FETCH data (RFC822.HEADER is
    *  returned).
    */
  val RFC822_HEADER : IMAPFetchContent = IMAPFetchContent("RFC822.HEADER")

  /**
    * The [RFC-2822] size of the message.
    */
  val RFC822_SIZE : IMAPFetchContent = IMAPFetchContent("RFC822.SIZE")


  /**
    * Functionally equivalent to BODY[TEXT], differing in the syntax
    * of the resulting untagged FETCH data (RFC822.TEXT is returned).
    */
  val RFC822_TEXT : IMAPFetchContent = IMAPFetchContent("RFC822.TEXT")

  /**
    * The unique identifier for the message.
    */
  val UID : IMAPFetchContent = IMAPFetchContent("UID")

}



case class BodySection(section: String) {
  /** allows for child notaion, i.e. 1.TEXT **/
  def  child(child: BodySection): BodySection =
    BodySection(s"$section.${child.section}")
}


object BodySection {

  /** full header of the message **/
  val HEADER: BodySection = BodySection("HEADER")

  /** onyl specified header fields **/
  def HeaderFields(names: String*): BodySection =
    BodySection(s"HEADER.FIELDS(${names.mkString(" ")})")

  /** all header fields except ones specified **/
  def HeaderFieldsNot(names: String*): BodySection =
    BodySection(s"HEADER.FIELDS.NOT(${names.mkString(" ")})")

  /** full header of the message **/
  val TEXT: BodySection = BodySection("HEADER")

  /** defines section of mime multipart **/
  def Section(num: Int):BodySection =
    BodySection(num.toString)



}
