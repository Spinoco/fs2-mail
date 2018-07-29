package spinoco.fs2.mail

import cats.effect.Concurrent
import fs2.Stream
import fs2.io.tcp.Socket
import scodec.Codec
import spinoco.protocol.mail.EmailHeader
import spinoco.protocol.mail.header.codec.EmailHeaderCodec
import spinoco.protocol.mail.mime.MIMEHeader

import scala.concurrent.duration.FiniteDuration

package object smtp {


  /**
    * Creates SMTP client. Once stream is run, the client connects to server and expects server to
    * send its welcome message (i.e. 220 foo.com, SMTP server Ready)
    * @param socket   Socket to use fro SMTP Connection to server
    */
  def client[F[_]: Concurrent](
    socket: Socket[F]
    , initialHandshakeTimeout: FiniteDuration
    , sendTimeout: FiniteDuration
    , emailHeaderCodec: Codec[EmailHeader] = EmailHeaderCodec.codec(100 * 1024) // 100K max header size
    , mimeHeaderCodec: Codec[MIMEHeader] = EmailHeaderCodec.mimeCodec(10 * 1024) // 10K for mime shall be enough
  ): Stream[F, SMTPClient[F]] =
    SMTPClient.mk(socket, initialHandshakeTimeout, sendTimeout, emailHeaderCodec, mimeHeaderCodec)


}
