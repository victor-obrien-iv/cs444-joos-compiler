package Environment

import AST.TypeDecl

import scala.collection.mutable

case class Environment(types: Map[String, List[TypeDecl]] = Map.empty,
                       variables: Map[String, (TypeAugmented, Option[ExprAugmented])] = Map.empty,
                       methods: mutable.Map[MethodHeader, Option[BlockStmtAugmented]] = mutable.Map.empty,
                       constructors: mutable.Map[ConstructorHeader, BlockStmtAugmented] = mutable.Map.empty)

object Environment {
  def empty: Environment = Environment()
}