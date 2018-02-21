package spinoco.fs2.mail.internal

import org.scalacheck.Properties
import org.scalacheck.Prop._
import fs2.{Strategy, Stream, Task}
import spinoco.fs2.mail.internal

import scala.concurrent.duration._

object TakeThroughDrainSpec extends Properties("TakeThroughDrain"){

  implicit val S = Strategy.fromFixedDaemonPool(8)

  property("early-terminated.drain") = protect{

    // We will be checking if the three remains, but all 0 are gone.
    // In this test twos mean the data between tags
    // one means a tag and three means some other data
    val source = Stream[Task, Int](2, 2, 2, 2, 2, 2, 1, 3)

    fs2.async.unboundedQueue[Task, Int].flatMap{ queue =>

      (source.to(queue.enqueue).drain ++
        queue.dequeue.through(internal.takeThroughDrain(_ != 1)).take(2).drain ++
        Stream.eval(queue.dequeue1)
      ).runLast
    }.unsafeRunFor(10.second) ?= Some(3)
  }

  property("normal-termination.dont-drain") = protect{

    // We will be checking if the three remains, but all 0 are gone.
    // In this test twos mean the data between tags
    // one means a tag and three means some other data
    val source = Stream[Task, Int](2, 2, 2, 2, 2, 2, 1, 3)

    fs2.async.unboundedQueue[Task, Int].flatMap{ queue =>
      (source.to(queue.enqueue).drain ++
        queue.dequeue.through(internal.takeThroughDrain(_ != 1)).drain ++
        Stream.eval(queue.dequeue1)
        ).runLast
    }.unsafeRunFor(10.second) ?= Some(3)
  }

}
