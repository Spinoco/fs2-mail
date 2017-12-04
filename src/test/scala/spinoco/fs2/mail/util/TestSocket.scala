package spinoco.fs2.mail.util


import fs2._
import fs2.io.tcp.Socket
import fs2.util.Async
import fs2.util.syntax._
import org.scalacheck.Prop
import scodec.bits.ByteVector
import shapeless.tag.@@
import spinoco.fs2.mail.interop.ByteVectorChunk

import scala.concurrent.duration.FiniteDuration

trait TestSocket[F[_]] extends Socket[F] {

  /** check actual props collected **/
  def props: F[Prop]

}


object TestSocket {

  object SocketState extends Enumeration {
    val Open, Closed, EndOfInput, EndOfOutput = Value
  }


  def mk[F[_]](
    spec: SocketSpecification
  )(implicit F: Async[F]): F[TestSocket[F]] = {



    F.refOf[Prop](Prop.undecided) flatMap { propRef =>
    F.refOf[List[ServerAction]](spec.steps) flatMap { actionRef =>
    F.refOf[Either[F[Unit], (String @@ ServerAction, ByteVector)]](Left(F.pure(()))) flatMap { sendRef =>
    F.refOf[ByteVector](ByteVector.empty) flatMap { receiveRef =>
    F.refOf[SocketState.Value](SocketState.Open) map { stateRef =>

      def interpret(actions: List[ServerAction]): F[Unit] = {
        ???
      }

      def interpretNext(after: String @@ ServerAction): F[Unit] = {
        actionRef.modify { actions =>
          if (actions.headOption.exists(_.id == after)) actions.tail
          else actions
        } flatMap { c =>
          if (c.modified) interpret(c.now)
          else F.fail(new Throwable(s"Tried to interpret the actions, but action with $after is not at head: ${c.now}"))
        }
      }

      def receivedAction(label: String)(f: PartialFunction[ClientAction, String @@ ClientAction]): F[Unit] = {
        actionRef.get.map(_.headOption) flatMap {
          case Some(action) => action match {
            case ServerAction.Expect(id, actions, next) => interpret(next(actions.collectFirst(f)))
            case other => F.fail(new Throwable(s"Client performed $label, but server did not expect that action: $other"))
          }
          case None => F.fail(new Throwable(s"Client performed $label, but not actions are expected"))
        }
      }

      def read0(maxBytes: Int, exact: Boolean, timeout: Option[FiniteDuration]): F[Option[Chunk[Byte]]] = {
        stateRef.get flatMap {
          case SocketState.Closed => F.fail(new Throwable("Socket is closed, cannot read"))
          case SocketState.EndOfInput => F.pure(None)
          case _ =>
            F.ref[Unit] flatMap { signal =>
              sendRef.modify2[F[Option[Chunk[Byte]]]] {
                case Right((id, data)) =>
                  val took = data.take(maxBytes)
                  val rem = data.drop(took.size)
                  if (rem.isEmpty) {
                    if (exact && took.size < maxBytes) {
                      def get0: F[Option[Chunk[Byte]]] = {
                        interpretNext(id) >>
                        signal.get >>
                        read0(maxBytes - took.size.toInt, exact, timeout) map { _ map { ch => ByteVectorChunk(took ++ ByteVectorChunk.asByteVector(ch)) } orElse Some(ByteVectorChunk(took)) }
                      }
                      (Left(signal.setPure(())), get0)
                    }
                    else {
                      (Left(F.pure(())), interpretNext(id) >> F.pure(Some(ByteVectorChunk(took))))
                    }
                  } else {
                    (Right((id, rem)), F.pure(Some(ByteVectorChunk(took))))
                  }


                case Left(signal0) =>
                  def get0: F[Option[Chunk[Byte]]] = {
                    signal.get >>
                    read0(maxBytes, exact, timeout)
                  }
                  (Left(signal0 >> signal.setPure(())), get0)
              } flatMap { _._2 }
            }

        }
      }


      new TestSocket[F] {
        def read(maxBytes: Int, timeout: Option[FiniteDuration]) =
          read0(maxBytes, exact = false, timeout)

        def reads(maxBytes: Int, timeout: Option[FiniteDuration]) =
          Stream.repeatEval(read(maxBytes, timeout)).unNoneTerminate.flatMap(Stream.chunk)

        def readN(numBytes: Int, timeout: Option[FiniteDuration]) =
          read0(numBytes, exact = true, timeout)

        def endOfInput = receivedAction("EndOfInput") { case ClientAction.EndOfInput(id) => id }

        def endOfOutput = receivedAction("EndOfOutput") { case ClientAction.EndOfOutput(id) => id }

        def close = receivedAction("Close") { case ClientAction.Close(id) => id }

        def remoteAddress = F.pure(spec.remoteAddress)

        def localAddress = F.pure(spec.localAddress)

        def write(bytes: Chunk[Byte], timeout: Option[FiniteDuration]) = {
          stateRef.get flatMap {
            case SocketState.Closed => F.fail(new Throwable("Socket is closed, cannot write"))
            case SocketState.EndOfOutput => F.fail(new Throwable("Socket is closed for writing, cannot write"))
            case _ =>
              receiveRef.modify(_ ++ ByteVectorChunk.asByteVector(bytes)) flatMap { _ =>
                receivedAction("Write") { case ClientAction.Receive(id, _) => id }
              }
          }
        }

        def writes(timeout: Option[FiniteDuration]) =
          _.chunks.evalMap(ch => write(ch, timeout))

        def props: F[Prop] = propRef.get
      }

    }}}}}


  }




}
