package spinoco.fs2.mail.imap

import spinoco.protocol.mail.imap.BodyStructure._


object EmailBodyPart {

  /** Represents textual mime part body **/
  case class TextPart(
    partId: String
    , tpe: BodyTypeText
    , ext: Option[SingleBodyExtension]
  ) extends  EmailBodyPart { self =>
    def charsetName: Option[String] = {
      self.tpe.fields.params
      .collectFirst { case(key, name) if key.equalsIgnoreCase("CHARSET") => name }
    }
  }

  /** represent binary mime part body **/
  case class BinaryPart(
    partId: String
    , tpe: BodyTypeBasic
    , ext: Option[SingleBodyExtension]
  ) extends EmailBodyPart


  /** from the BodyParts extracts email body parts with their Id to be used later by fetch**/
  def flatten(body: BodyPart): Vector[EmailBodyPart] = {

    // yields Some() in case of text/binary part, None for email envelope
    def mkEmailPart(tpe: BodyType, ext: Option[SingleBodyExtension], ids: Vector[Int]): Option[EmailBodyPart] = {
      def id = ids.mkString(".")
      tpe match {
        case text: BodyTypeText => Some(TextPart(id, text, ext))
        case bin: BodyTypeBasic => Some(BinaryPart(id, bin, ext))
        case _ => None
      }
    }

    def go(ids: Vector[Int], rem: Vector[Option[BodyPart]], acc: Vector[EmailBodyPart]): Vector[EmailBodyPart] = {

      rem.headOption match {
        case Some(Some(part)) => part match {
          case multi: MultiBodyPart =>
            // start new hierarchy by setting index to 0
            val nextIds = ids.lastOption.map(idx => ids.init ++ Vector(idx + 1, 0)).getOrElse(Vector(0))
            go(nextIds, multi.parts.map(Some(_)) ++ (None +: rem.tail), acc)

          case single: SingleBodyPart =>
            // add to hiearachy, increment index move to next
            val nextIds = ids.lastOption.map(idx => ids.init :+ (idx + 1)).getOrElse(Vector(1))
            mkEmailPart(single.tpe, single.ext, nextIds) match {
              case Some(part) => go(nextIds, rem.tail, acc :+ part)
              case None => go(nextIds, rem.tail, acc)
            }
        }

        case Some(None) =>
          // indicates last of tree hierarchy move up
          go(ids.init, rem.tail, acc)

        case None =>
          // finished
          acc
      }
    }

    go(Vector.empty, Vector(Some(body)), Vector.empty)
  }

}



sealed trait EmailBodyPart



