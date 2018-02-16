package AST

import Parser.TreeNode
import akka.actor.Actor

class AstActor extends Actor{

  val builder = new AstBuilder()

  override def receive: Receive = {
    case parseTree: TreeNode =>
      sender ! builder.buildCompilationUnit(parseTree)
  }
}
