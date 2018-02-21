package spinoco.fs2.mail.imap

import fs2.{Strategy, Stream, Task}
import fs2.util.Async
import fs2.util.syntax._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import spinoco.fs2.mail.imap.IMAPClient.impl.{IMAPData, IMAPText}

object IMAPClientCommandSpec extends Properties("IMAPClient.request") {

  implicit val S = Strategy.fromFixedDaemonPool(8)
  implicit val F = implicitly[Async[Task]]

  def createTagged(shouldFail: Task[Boolean], count: Int, done: Task[Unit]): Stream[Task, IMAPData] = {
    Stream.unfoldEval(count){ s =>
      shouldFail.flatMap{
        case false if s > 0 => F.pure(Some(IMAPText(s"* $s") -> (s - 1)))
        case false => done >> F.pure(None)
        case true => F.fail(new Throwable("Closed before drained"))
      }
    }
  }

  property("cmd.release.after.drain") = protect{
    val (drained, result) =
      F.refOf(1l).flatMap{ idxRef =>
      F.refOf(false).flatMap{ drainedRef =>
      fs2.async.semaphore(1).flatMap{ gate =>

        val tagged = createTagged(gate.available.map(_ == 0).map(!_), 3, drainedRef.setPure(true))

        IMAPClient.impl.requestCmd[Task](idxRef, gate, tagged, _ => F.pure(()))(IMAPCommand.Logout).flatMap{
          case Left(_) => Stream.empty
          case Right(next) => next
        }.take(2).runLog.flatMap{ result =>
          drainedRef.get.map(_ -> result)
        }
      }}}.unsafeRun()

      (result ?= Vector(IMAPText("* 3"), IMAPText("* 2"))) && drained
    }

}
