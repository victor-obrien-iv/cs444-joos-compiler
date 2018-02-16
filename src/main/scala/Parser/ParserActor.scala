package Parser

import Lalr.Lalr
import Token.Token
import akka.actor.{Actor, ActorRef}

class ParserActor(lalr: Lalr, filename: String, reporter: ActorRef) extends Actor {

  val parser: Parser = new Parser(lalr, filename)

  override def receive: Receive = {
    case t: List[Token] =>
      try {
        val parseTree: TreeNode = parser.parse(t)
        sender ! parseTree
      } catch {
        case e: Error.Error =>
          reporter ! e
          sender ! e
      }
  }
}
