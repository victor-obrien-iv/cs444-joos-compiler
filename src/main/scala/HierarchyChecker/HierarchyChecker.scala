package HierarchyChecker

import AST._

import scala.collection.mutable
import scala.concurrent.Future

class HierarchyChecker(localContexts: Map[CompilationUnit, Map[String, TypeDecl]], typeContext: Map[String, List[TypeDecl]]) {

  def checkForCycles(): Future[Unit] = {
    /**
      *  The hierarchy must be acyclic
      */
    val checkedUnits: mutable.Set[TypeDecl] = mutable.Set() // memoization bc/ dynamic programming is hard
    val localContextsTransformed = localContexts.map( context => context._1.typeDecl -> context._2)
    def checkIfCyclic(unit: TypeDecl, visited: List[TypeDecl]): Unit = {
      if (visited.contains(unit))
        throw Error.Error(visited.map(decl => decl.name.lexeme).mkString(" -> "),
          "The hierarchy is cyclic", Error.Type.HierarchyCheck)

      if (! checkedUnits.contains(unit)) {
        unit match {
          case i: InterfaceDecl =>
            for(extend <- i.extensionOf)
              checkIfCyclic(localContextsTransformed(unit)(extend.name), visited.::(unit))

          case c: ClassDecl =>
            c.extensionOf match {
              case Some(extend) =>
                checkIfCyclic(localContextsTransformed(unit)(extend.name), visited.::(unit))
              case None =>
            }
            for(imp <- c.implementationOf)
              checkIfCyclic(localContextsTransformed(unit)(imp.name), visited.::(unit))
        }

        checkedUnits += unit
      }
    }
    for (unit <- localContexts.keys.toList){
      checkIfCyclic(unit.typeDecl, List())
    }
    Future.successful(())
  }


  def check(ast: CompilationUnit): List[Future[Unit]] = {
    val passes: List[Visitor] = List(
      new ExtendsPass(localContexts(ast), typeContext, ast),
      new MethodsPass(localContexts, typeContext, ast)
    )
    for(pass <- passes) yield pass.run(ast)
  }
}
