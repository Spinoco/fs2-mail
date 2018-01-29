package spinoco.fs2.mail.imap

import fs2.Task
import fs2.Stream
import org.scalacheck.Properties
import org.scalacheck.Prop._
import shapeless.tag
import spinoco.fs2.mail.imap.IMAPClient.impl.IMAPText

object IMAPIdleSpec extends Properties("IMAPIdleSpec"){

  val encoded = List(
    "* 3 EXISTS"
    , "+ idling"
    , "* 6 EXPUNGE"
    , "* 6 OTHER"
  )

  property("decode-commands") = protect {
    IMAPIdleContext.impl.makeIdleEvents[Task](
      Stream.emits(encoded.map(IMAPText.apply))
    ).runLog.unsafeRun() ?= Vector(
      IMAPIdleMessage.Exists(tag[MailUID](3))
      , IMAPIdleMessage.Expunge(tag[MailUID](6))
      , IMAPIdleMessage.CustomMessage("OTHER", tag[MailUID](6))
    )
  }

  property("decode-commands.fail.spacing") = protect {
    IMAPIdleContext.impl.makeIdleEvents[Task](
      Stream.emit(IMAPText("* 3EXISTS"))
    ).runLog.attempt.unsafeRun().isLeft
  }

  property("decode-commands.fail.start") = protect {
    IMAPIdleContext.impl.makeIdleEvents[Task](
      Stream.emit(IMAPText("3 EXISTS"))
    ).runLog.attempt.unsafeRun().isLeft
  }


}
