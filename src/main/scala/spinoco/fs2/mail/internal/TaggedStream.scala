package spinoco.fs2.mail.internal

import fs2.Stream
import fs2.util.Async
import fs2.util.syntax._

/**
  * Describes a stream that contains continuous tagged data.
  */
trait TaggedStream[F[_], A] {

  /**
    * Behaves exactly as [[Stream.takeThrough]], but ensures that even when the
    * resulting stream is interrupted/ends early, then the source stream is read
    * until the toggle returns false.
    *
    * @param toggle The function that determines up to where we want to read data
    *               from the source stream.
    */
  def takeThrough(
    toggle: A => Boolean
  ): Stream[F, A]

}


object TaggedStream {

  /**
    * Makes a [[TaggedStream]] from a stream. Do note that the behaviour of the resulting tagged stream
    * depends on how the source stream is build. To ensure persistence over the fully
    * drained blocks this should be backed up by some sort of queue.
    *
    * @param source The source stream from which we will be reading data.
    */
  def fromStream[F[_], A](source: Stream[F, A])(implicit F: Async[F]): TaggedStream[F, A] = {
    new TaggedStream[F, A] {
      def takeThrough(toggle: A => Boolean): Stream[F, A] = {
        Stream.eval(fs2.async.synchronousQueue[F, Option[A]]).flatMap{ feedQueue =>
        Stream.eval(F.refOf(false)).flatMap{ finishedRef =>
          Stream.eval(F.start(
            source.takeThrough(toggle).zipWithNext.evalMap{
              case (current, None) => feedQueue.enqueue1(Some(current)) >> finishedRef.setPure(true) >> feedQueue.enqueue1(None)
              case (current, Some(_)) => feedQueue.enqueue1(Some(current))
            }.run
          )).flatMap(_ =>
            feedQueue.dequeue.unNoneTerminate
          ).onFinalize{
            finishedRef.get.flatMap{
              case true => F.pure(())
              case false => feedQueue.dequeue.unNoneTerminate.drain.run
            }
          }
        }}
      }
    }
  }



}