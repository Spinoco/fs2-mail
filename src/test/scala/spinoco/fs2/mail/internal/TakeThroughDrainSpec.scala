package spinoco.fs2.mail.internal

import cats.effect.IO
import org.scalacheck.Properties
import org.scalacheck.Prop._

import fs2._
import spinoco.fs2.mail.internal
import scala.concurrent.duration._

object TakeThroughDrainSpec extends Properties("TakeThroughDrain"){

  import scala.concurrent.ExecutionContext.Implicits.global

  property("early-terminated.drain") = protect{

    // We will be checking if the three remains, but all 0 are gone.
    // In this test twos mean the data between tags
    // one means a tag and three means some other data
    val source = Stream[Int](2, 2, 2, 2, 2, 2, 1, 3).covary[IO]

    fs2.async.unboundedQueue[IO, Int].flatMap{ queue =>

      (source.to(queue.enqueue).drain ++
        queue.dequeue.through(internal.takeThroughDrain(_ != 1)).take(2).drain ++
        Stream.eval(queue.dequeue1)
      ).compile.last
    }.unsafeRunTimed(10.second).flatten ?= Some(3)
  }

  property("normal-termination.dont-drain") = protect{

    // We will be checking if the three remains, but all 0 are gone.
    // In this test twos mean the data between tags
    // one means a tag and three means some other data
    val source = Stream[Int](2, 2, 2, 2, 2, 2, 1, 3).covary[IO]

    fs2.async.unboundedQueue[IO, Int].flatMap{ queue =>
      (source.to(queue.enqueue).drain ++
        queue.dequeue.through(internal.takeThroughDrain(_ != 1)).drain ++
        Stream.eval(queue.dequeue1)
        ).compile.last
    }.unsafeRunTimed(10.second).flatten ?= Some(3)
  }

  val Boom = new Throwable("Boom")

  property("propagate-failure-from-source") = protect {
    val source = Stream[Int](2, 2, 2, 2, 2, 2, 1, 3).covary[IO] ++ Stream.raiseError(Boom)

    fs2.async.unboundedQueue[IO, Int].flatMap{ queue =>
      (source.to(queue.enqueue).drain ++
        queue.dequeue.through(internal.takeThroughDrain(_ != 100)).drain ++
        Stream.eval(queue.dequeue1)
        ).compile.last
    }.attempt.unsafeRunTimed(10.second) ?= Some(Left(Boom))
  }

  property("propagate-failure-on-finalize") = protect {
    val source = Stream[Int](2, 2, 2, 2, 2, 2, 1, 3).covary[IO] ++ Stream.raiseError(Boom)

    fs2.async.unboundedQueue[IO, Int].flatMap{ queue =>
      (source.to(queue.enqueue).drain ++
        queue.dequeue.through(internal.takeThroughDrain(_ != 1)).drain ++
        Stream.eval(queue.dequeue1)
        ).compile.last
    }.attempt.unsafeRunTimed(10.second) ?= Some(Left(Boom))
  }

}
