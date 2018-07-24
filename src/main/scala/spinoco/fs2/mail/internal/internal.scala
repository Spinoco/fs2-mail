package spinoco.fs2.mail

import cats.syntax.all._
import cats.effect.Effect
import fs2._

import scala.concurrent.ExecutionContext

package object internal {

  /**
    * Behaves like [[Stream.takeThrough]], but resulting stream will not end until `f` yields to false,
    * draining all elements where `f` still yields to true.
    *
    * This is done by running the source stream in parallel to the consume stream. The messages read from the
    * source stream are then published into a synchronous queue, that blocks until the down stream reads from it.
    *
    * Once downstream finishes this synchronous queue is read until exhaustion marked by [[None]].
    */
  def takeThroughDrain[F[_] : Effect, A](predicate: A => Boolean)(
    implicit EC: ExecutionContext
  ): Pipe[F, A, A] = { source =>
    Stream.eval(fs2.async.synchronousQueue[F, Option[Either[Throwable, A]]]).flatMap{ feedQueue =>

      // dequeue and propagate errors to downstream
      def dequeue:Stream[F, A] = {
        feedQueue.dequeue.unNoneTerminate.flatMap {
          case Left(err) => Stream.raiseError(err)
          case Right(a) => Stream.emit(a)
        }
      }

      // Finished flag, to prevent reading of the synchronous queue in case
      // the down stream consumed all data available to it.
      Stream.eval(async.refOf[F, Boolean](false)).flatMap{ finishedRef =>
        Stream.eval(async.start(
          source.takeThrough(predicate)
          .evalMap( a => feedQueue.enqueue1(Some(Right(a))))
          .compile.drain.attempt.flatMap { r =>
            finishedRef.setSync(true) >> feedQueue.enqueue1(r.left.toOption.map(Left(_)))
          }
        )).flatMap(_ =>
          dequeue
        ).onFinalize{
          finishedRef.get.flatMap{
            case true => Effect[F].unit
            case false => dequeue.compile.drain
          }
        }
      }}
  }

}