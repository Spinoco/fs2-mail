package spinoco.fs2.mail.encoding

import org.scalacheck.Prop._
import org.scalacheck.Properties
import scodec.Attempt
import spinoco.fs2.mail.encoding


object encodingSpec extends Properties("encodingSpec") {


  property("encode-decode-imap.utf7") = forAll { (str: String) =>
    (encoding.encodeIMAPUtf7(str) flatMap { encoded =>
    encoding.decodeIMAPUtf7(encoded) map { decoded =>
      (decoded ?= str) :| s"In: $str, Encoded: $encoded decoded: $decoded"
    }}).getOrElse(falsified)
  }


  property("encode-decode-imap.utf7.&") = protect {
    encoding.encodeIMAPUtf7("&") ?= Attempt.successful("&-")
  }


}
