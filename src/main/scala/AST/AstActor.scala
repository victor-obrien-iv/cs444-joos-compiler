package AST

import Parser.TreeNode
import akka.actor.{Actor, ActorRef}

class AstActor(filename: String, reporter: ActorRef) extends Actor{

  val builder = new AstBuilder(filename)

  override def receive: Receive = {
    case parseTree: TreeNode =>
      try {
        sender ! builder.buildCompilationUnit(parseTree)
      } catch {
        case e =>
          sender ! e
          reporter ! e
      }
    case x =>
      sender ! "why did you give me that??"
  }
}
