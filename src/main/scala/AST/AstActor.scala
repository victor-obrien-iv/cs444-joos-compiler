package AST

import Parser.TreeNode
import akka.actor.Actor

class AstActor extends Actor{

  val builder = new AstBuilder()

  override def receive: Receive = {
    case parseTree: TreeNode =>
      try {
        sender ! builder.buildCompilationUnit(parseTree)
      } catch {
        case e =>
          println(e.printStackTrace())
          sender ! e
      }
    case x =>
      sender ! "why did you give me that??"
  }
}
