package spinoco.fs2.mail.imap

import cats.effect.IO
import cats.effect.concurrent.{Ref, Semaphore}
import fs2._
import cats.syntax.all._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import spinoco.fs2.mail.imap.IMAPClient.impl.{IMAPData, IMAPText}

import scala.concurrent.ExecutionContext

object IMAPClientCommandSpec extends Properties("IMAPClient.request") {

  private implicit val cs = IO.contextShift(ExecutionContext.global)
  private implicit val c = IO.ioConcurrentEffect

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
      Ref.of[IO, Long](1l).flatMap { idxRef =>
      Ref.of[IO, Boolean](false).flatMap{ drainedRef =>
      Semaphore[IO](1).flatMap{ gate =>

        val tagged = createTagged(gate.available.map(_ == 0).map(!_), 3, drainedRef.set(true))

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
