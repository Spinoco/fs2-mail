package spinoco.fs2.mail.imap

import org.scalacheck.Properties
import org.scalacheck.Prop._
import shapeless.tag

object IMAPMailboxStatusSpec extends Properties("IMAPMailboxStatus") {

  property("decode") = protect {

    IMAPMailboxStatus.parse(Seq(
      " FLAGS (\\Answered \\Flagged \\Draft \\Deleted \\Seen $Forwarded $Junk $NotJunk $NotPhishing $Phishing JunkRecorded NotJunk)"
      , " OK [PERMANENTFLAGS (\\Deleted \\Seen $Forwarded $Junk)] Flags permitted."
      , " OK [UIDVALIDITY 663758749] UIDs valid."
      , " 4081 EXISTS"
      , " 10 RECENT"
      , " OK [UIDNEXT 4100] Predicted next UID."
      , " OK [HIGHESTMODSEQ 646785]"
      , " OK [UNSEEN 99]"
    )) ?= IMAPMailboxStatus(
      flags = Seq(
        "\\Answered"
        , "\\Flagged"
        , "\\Draft"
        , "\\Deleted"
        , "\\Seen"
        , "$Forwarded"
        , "$Junk"
        , "$NotJunk"
        , "$NotPhishing"
        , "$Phishing"
        , "JunkRecorded"
        , "NotJunk"
      )
      , permanentFlags = Seq(
        "\\Deleted"
        , "\\Seen"
        , "$Forwarded"
        , "$Junk"
      )
      , exists = 4081
      , recent = 10
      , unseen = Some(99)
      , uidNext = tag[MailUID](4100)
      , uidValidity = Some(663758749)
    )

  }

}
