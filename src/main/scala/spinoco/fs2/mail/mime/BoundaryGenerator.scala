package spinoco.fs2.mail.mime

import java.util.UUID


/** generator of uniqe boundary strings for mime multiparts **/
trait BoundaryGenerator {

  /** returns next boundary string **/
  def next: String

}


object BoundaryGenerator {

  val default: BoundaryGenerator = {
    new BoundaryGenerator {
      def next: String =  s"---${UUID.randomUUID()}---${UUID.randomUUID()}---"
    }
  }

}