package Environment

case class Environment(variables: List[(String, DeclAugmented)], methods: List[(String, List[MethodDeclAugmented])])