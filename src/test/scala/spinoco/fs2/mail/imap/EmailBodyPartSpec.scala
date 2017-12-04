package spinoco.fs2.mail.imap

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spinoco.fs2.mail.imap.EmailBodyPart.{BinaryPart, TextPart}
import spinoco.protocol.mail.imap.BodyStructure._

object EmailBodyPartSpec extends Properties("EmailBodyPart") {

  val text = BodyTypeText("PLAIN", BodyFields(Vector.empty, None, None, "", 0), 0)
  val bin = BodyTypeBasic(BasicMedia("img", "png"), BodyFields(Vector.empty, None, None, "", 0))
  val singleTextBody = SingleBodyPart(text, None)
  val singleBinBody = SingleBodyPart(bin, None)
  val multiBody = MultiBodyPart(Vector.empty, "related", None)

  property("flatten.empty") = protect {
    EmailBodyPart.flatten(multiBody) ?= Vector.empty
  }

  property("flatten.single.text") = protect {
    EmailBodyPart.flatten(singleTextBody) ?= Vector(
      TextPart("1", text, None)
    )
  }

  property("flatten.single.bin") = protect {
    EmailBodyPart.flatten(singleBinBody) ?= Vector(
      BinaryPart("1", bin, None)
    )
  }

  property("flatten.multi-one-level") = protect {

    EmailBodyPart.flatten(
      multiBody.copy(
        parts = Vector(
          singleTextBody
          , singleBinBody
        )
      )
    ) ?= Vector(
      TextPart("1", text, None)
      , BinaryPart("2", bin, None)
    )
  }

  property("flatten.multi-one-level.nested") = protect {
    EmailBodyPart.flatten(
      multiBody.copy( parts = Vector(
        singleTextBody
        , multiBody.copy( parts = Vector(
          singleTextBody
          , singleBinBody
          , multiBody.copy( parts = Vector(
            singleTextBody
            , singleBinBody
          ))
        ))
        , singleBinBody
      ))
    ) ?= Vector(
      TextPart("1", text, None)
      , TextPart("2.1", text, None)
      , BinaryPart("2.2", bin, None)
      , TextPart("2.3.1", text, None)
      , BinaryPart("2.3.2", bin, None)
      , BinaryPart("3", bin, None)
    )
  }


}
