package Parser

import Token.Token

case class TreeNode(state: Either[Token, String], children: List[TreeNode])

case class ParseError(message: String) extends Exception
