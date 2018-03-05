package HierarchyChecker

import AST.{CompilationUnit, TypeDecl, Visitor}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

class HierarchyChecker(context: Map[String, List[TypeDecl]],
                       private val typeContext: Map[String, List[TypeDecl]]) {
  private val localContext = context.map { name => name._1 -> name._2.head }

  def check(ast: CompilationUnit): List[Future[Unit]] ={
    val passes: List[Visitor] = List(
      new ExtendsPass(localContext, typeContext, ast)
    )
    for(pass <- passes) yield pass.run(ast)
  }
}
