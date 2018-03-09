package Environment

import scala.collection.mutable

//TODO: Incorporate type-linking into environment
case class Environment(variables: List[((TypeAugmented, String), Option[ExprAugmented])],
                       methods: mutable.Map[MethodHeader, Option[BlockStmtAugmented]],
                       constructors: mutable.Map[ConstructorHeader, BlockStmtAugmented])

object Environment {
  def empty: Environment = Environment(Nil, mutable.Map.empty, mutable.Map.empty)
}