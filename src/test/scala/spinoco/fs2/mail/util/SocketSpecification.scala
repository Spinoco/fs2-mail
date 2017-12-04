package spinoco.fs2.mail.util

import java.net.SocketAddress

import org.scalacheck.Prop
import scodec.{Attempt, DecodeResult}
import scodec.bits.ByteVector
import shapeless.tag.@@

import scala.concurrent.duration.FiniteDuration

case class SocketSpecification(
  steps: List[ServerAction]
  , localAddress: SocketAddress
  , remoteAddress: SocketAddress
)


object SocketSpecification {



}


sealed trait ServerAction {
  def id: String @@ ServerAction
}


sealed trait ClientAction {
  def id: String @@ ClientAction
}

object ClientAction {
  // Signals the socket is closed
  case class Close(id: String @@ ClientAction) extends ClientAction
  // Signals End of Input
  case class EndOfInput(id: String @@ ClientAction) extends ClientAction
  // Signals End of Output
  case class EndOfOutput(id: String @@ ClientAction) extends ClientAction

  /**
    * Await receiving of supplied data
    * @param check    Checks received data, eventually the property.
    *                 if the result fails, the system awaits next data from the client and then applies check until
    *                 Attempt is successful. Result may return remainder
    */
  case class Receive(
    id: String @@ ClientAction
    , check: ByteVector => Attempt[DecodeResult[Prop]]
  ) extends ClientAction
}


object ServerAction {

  // Signals the socket is closed
  case class Close(id: String @@ ServerAction) extends ServerAction
  // Signals End of Input
  case class EndOfInput(id: String @@ ServerAction) extends ServerAction
  // Signals End of Output
  case class EndOfOutput(id: String @@ ServerAction) extends ServerAction


  /**
    * Sends supplied data to remote party
    *
    * Next step is entered AFTER all data was consumed by client.
    *
    * @param data   Data to send
    */
  case class SendData(id: String @@ ServerAction, data: ByteVector) extends ServerAction


  /**
    * Schedules an asynchronous timeout that will check whether some actions are done, and eventually schedule `next` operation.
    * Control is going to next action immediately once this is scheduled.
    * @param timeout  Timeout when this will be triggered
    * @param done     Check, that following props were done when timeout occurred
    * @param next     Next actions to take once timeout is scheduled. Will be consulted only if steps in `done` did complete.
    *                 If `done` is empty, will be always consulted.
    */
  case class Timeout(
    id: String @@ ServerAction
    , timeout: FiniteDuration
    , done: Set[String @@ ServerAction]
    , next: List[ServerAction]
  ) extends ServerAction


  /**
    * Expect given client action. Will move to next step when given action is performed by client
    * @param actions  Any actions allowed at this stage. If the action from the client is different it is considered as failure.
    * @param next     Consulted to run next socket action based on client's action.
    */
  case class Expect(
    id: String @@ ServerAction
    , actions: List[ClientAction]
    , next: Option[(String @@ ClientAction)] => List[ServerAction]
  ) extends ServerAction

  /**
    * Causes to fail the server interpretation. Socket will be failed, any operation wil be stopped and yielding with supplied failure.
    *
    * @param id       Id of this operation
    * @param rsn      Reason for the failure.
    */
  case class Fail(
    id: String @@ ServerAction
    , rsn: Throwable
  ) extends ServerAction

}