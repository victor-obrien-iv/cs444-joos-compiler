package Parser

import java.io.{FileInputStream, ObjectInputStream}

import Lalr.Lalr
import Token._
import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers{

  val lalrSteam = new ObjectInputStream(new FileInputStream("src/test/resources/lalr"))

  val lalr: Lalr = lalrSteam.readObject().asInstanceOf[Lalr]

  val basicClassTest = List(JavaClass(row = 0, col = 0), Identifier("a", 0, 0), LBrace(row = 0, col = 0), RBrace(row = 0, col = 0))
  val parser = new Parser(lalr)

  "Parser" should "parse correct java programs" in {
    parser.parse(basicClassTest).state shouldBe "CompilationUnit"
  }
}
