package spinoco.fs2.mail.internal

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalacheck.Properties
import org.scalacheck.Prop._

import fs2._
import cats.effect.std.Queue
import spinoco.fs2.mail.internal
import scala.concurrent.duration._

object TakeThroughDrainSpec extends Properties("TakeThroughDrain"){


  property("early-terminated.drain") = protect{

    // We will be checking if the three remains, but all 0 are gone.
    // In this test twos mean the data between tags
    // one means a tag and three means some other data
    val source = Stream[IO, Int](2, 2, 2, 2, 2, 2, 1, 3).covary[IO]

    Stream.eval(Queue.unbounded[IO, Int]).flatMap{ queue =>
      (source.evalMap(queue.offer).drain ++
        Stream.repeatEval(queue.take).through(internal.takeThroughDrain(_ != 1)).take(2).drain ++
        Stream.eval(queue.take)
      )
    }.compile.last.timeout(10.second).unsafeRunSync() ?= Some(3)
  }

  property("normal-termination.dont-drain") = protect{

    // We will be checking if the three remains, but all 0 are gone.
    // In this test twos mean the data between tags
    // one means a tag and three means some other data
    val source = Stream[IO, Int](2, 2, 2, 2, 2, 2, 1, 3).covary[IO]

    Stream.eval(Queue.unbounded[IO, Int]).flatMap{ queue =>
      (source.evalMap(queue.offer).drain ++
        Stream.repeatEval(queue.take).through(internal.takeThroughDrain(_ != 1)).drain ++
        Stream.eval(queue.take)
      )
    }.compile.last.timeout(10.second).unsafeRunSync() ?= Some(3)
  }

  val Boom = new Throwable("Boom")

  property("propagate-failure-from-source") = protect {
    val source = Stream[IO, Int](2, 2, 2, 2, 2, 2, 1, 3).covary[IO] ++ Stream.raiseError[IO](Boom)

    Stream.eval(Queue.unbounded[IO, Int]).flatMap{ queue =>
      (source.evalMap(queue.offer).drain ++
        Stream.repeatEval(queue.take).through(internal.takeThroughDrain(_ != 100)).drain ++
        Stream.eval(queue.take)
      )
    }.compile.last.attempt.timeout(10.second).unsafeRunSync() ?= Left(Boom)
  }

  property("propagate-failure-on-finalize") = protect {
    val source = Stream[IO, Int](2, 2, 2, 2, 2, 2, 1, 3).covary[IO] ++ Stream.raiseError[IO](Boom)

    Stream.eval(Queue.unbounded[IO, Int]).flatMap{ queue =>
      (source.evalMap(queue.offer).drain ++
        Stream.repeatEval(queue.take).through(internal.takeThroughDrain(_ != 1)).drain ++
        Stream.eval(queue.take)
      )
    }.compile.last.attempt.timeout(10.second).unsafeRunSync() ?= Left(Boom)
  }

}
