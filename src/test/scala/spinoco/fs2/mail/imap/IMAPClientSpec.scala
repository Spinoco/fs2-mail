package spinoco.fs2.mail.imap

import java.net.InetSocketAddress
import java.nio.channels.AsynchronousChannelGroup
import java.util.concurrent.Executors
import javax.net.ssl.SSLContext

import fs2._
import fs2.crypto.TLSEngine
import fs2.crypto.io.tcp.TLSSocket
import shapeless.tag

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

/**
  * Created by pach on 11/10/17.
  */
object IMAPClientSpec extends App {

  val ES = Executors.newCachedThreadPool(Strategy.daemonThreadFactory("AG"))
  implicit val AG = AsynchronousChannelGroup.withThreadPool(ES)
  implicit val S = Strategy.fromExecutionContext(ExecutionContext.Implicits.global)
  implicit val Sch = Scheduler.fromFixedDaemonPool(8)


  println("STARTING THAT")

  val ctx = SSLContext.getInstance("TLS")
  ctx.init(null, null, null)
  val clientTLS = {
    val eng = ctx.createSSLEngine()
    eng.setUseClientMode(true)
    eng
  }


  io.tcp.client[Task](new InetSocketAddress("imap.gmail.com", 993)).flatMap { tcpSocket =>
  Stream.eval(TLSEngine[Task](clientTLS)) flatMap { tlsEngine =>
  Stream.eval(TLSSocket[Task](tcpSocket, tlsEngine)) flatMap { tlsSocket =>
  IMAPClient.mk[Task](tlsSocket) flatMap { imapClient =>
    Stream.eval(imapClient.login("EMAIL", "PASSWORD")) flatMap { loginR =>  println(s"XXXY LOGIN: $loginR")
    Stream.eval(imapClient.capability) flatMap { capR =>  println(s"XXXY CAP: $capR")
    Stream.eval(imapClient.list("", "*")) flatMap { listR =>  println(s"XXXY LIST: $listR")
    Stream.eval(imapClient.select(tag[MailboxName]("INBOX"))) flatMap { inboxStatus => println(s"XXXY MBOX: $inboxStatus")
    Stream.eval(imapClient.search(IMAPSearchTerm.Header("Message-ID", "CANdT_=6g=mMJzpOTVaWFXnkW619LtX5uH02RTXD=u5qq-_RRvw@mail.gmail.com"))) flatMap { rslt =>  println(s"SEARCHRSLT: $rslt")



      imapClient.emailHeaders(4200l to 4248l).map { hdr => println(s"HDR: $hdr") } ++
      time.sleep[Task](1.minute)

    }}}}}

  }}}}
  .run.unsafeRun



}
