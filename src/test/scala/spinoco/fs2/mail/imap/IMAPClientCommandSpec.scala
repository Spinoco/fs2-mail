package spinoco.fs2.mail.imap

import cats.effect.IO
import fs2._
import cats.syntax.all._
import org.scalacheck.Properties
import org.scalacheck.Prop._

import spinoco.fs2.mail.imap.IMAPClient.impl.{IMAPData, IMAPText}

object IMAPClientCommandSpec extends Properties("IMAPClient.request") {

  import scala.concurrent.ExecutionContext.Implicits.global

  def createTagged(shouldFail: IO[Boolean], count: Int, done: IO[Unit]): Stream[IO, IMAPData] = {
    Stream.unfoldEval(count){ s =>
      shouldFail.flatMap{
        case false if s > 0 => IO.pure(Some(IMAPText(s"* $s") -> (s - 1)))
        case false => done >> IO.pure(None)
        case true => IO.raiseError(new Throwable("Closed before drained"))
      }
    }
  }

  property("cmd.release.after.drain") = protect{
    val (drained, result) =
      async.refOf[IO, Long](1l).flatMap { idxRef =>
      async.refOf[IO, Boolean](false).flatMap{ drainedRef =>
      fs2.async.semaphore[IO](1).flatMap{ gate =>

        val tagged = createTagged(gate.available.map(_ == 0).map(!_), 3, drainedRef.setSync(true))

        IMAPClient.impl.requestCmd[IO](idxRef, gate, tagged, _ => IO.unit)(IMAPCommand.Logout).flatMap{
          case Left(_) => Stream.empty
          case Right(next) => next
        }.take(2).compile.toVector.flatMap { result =>
          drainedRef.get.map(_ -> result)
        }
      }}}.unsafeRunSync()

      (result ?= Vector(IMAPText("* 3"), IMAPText("* 2"))) && drained
    }

}
