package spinoco.fs2.mail.imap

import org.scalacheck.Properties
import org.scalacheck.Prop._
import shapeless.tag
import spinoco.fs2.mail.imap.IMAPCommand._


object IMAPCommandSpec extends Properties("IMAPCommand"){

  property("login") = protect{

    LoginPlainText("user1", "pass1").asIMAPv4 ?= "LOGIN user1 \"pass1\""
  }

  property("logout") = protect{

    Logout.asIMAPv4 ?= "LOGOUT"
  }

  property("capability") = protect{

    Capability.asIMAPv4 ?= "CAPABILITY"
  }

  property("listMailbox") = protect{

    ListMailbox("base", "match").asIMAPv4 ?= """LIST "base" "match""""
  }

  property("examine") = protect{

    Examine(tag[MailboxName]("box1")).asIMAPv4 ?= """EXAMINE "box1""""
  }

  property("select") = protect{

    Select(tag[MailboxName]("box2")).asIMAPv4 ?= """SELECT "box2""""
  }

  property("search.no-charset") = protect{

    Search(None, IMAPSearchTerm.ALL).asIMAPv4 ?= "SEARCH ALL"
  }

  property("search.charset") = protect{

    Search(Some("UTF-8"), IMAPSearchTerm.ALL).asIMAPv4 ?= "SEARCH CHARSET UTF-8 ALL"
  }

  property("fetch.exact") = protect{

    Fetch(1l to 1l, Nil).asIMAPv4 ?= "FETCH 1 ()"
  }

  property("fetch.range") = protect{

    Fetch(1l to 10l, Nil).asIMAPv4 ?= "FETCH 1:10 ()"
  }

}
