package Parser

case class TreeNode(state: String, children: List[TreeNode])

case class ParseError(message: String) extends Exception
