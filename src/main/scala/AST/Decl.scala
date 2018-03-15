package AST

import Token.{Identifier, Modifier}

/**
  * Decl represents a declaration
  */
trait Decl extends AstNode

/**
  * CompilationUnit is a special declaration that encapsulates everything
  *   in a .java file
  * @param fileName the path and file that is associated with this compilation unit
  * @param packageName the identifier that comes after 'package' in the source code
  *                    or None if no package declaration exists for this file
  * @param imports the list of import declarations in this file
  * @param typeDecl the type declared in the compilation unit
  */
case class CompilationUnit(fileName: String, packageName: Option[FullyQualifiedID],
                           imports: List[ImportDecl], typeDecl: TypeDecl) extends Decl

/**
  * ImportDecl represents an 'import' usage in the source code
  * @param name the identifier for the import
  * @param asterisk if the import had the asterisk wildcard at the end of it
  */
case class ImportDecl(name: FullyQualifiedID, asterisk : Boolean) extends Decl

/**
  * Represents a Type
  */
sealed trait TypeDecl extends Decl {
  def modifiers: List[Token.Modifier]
  def name: Token.Identifier
  def members: List[Decl]
  def id: Int
}
/**
  * InterfaceDecl represents an interface declaration
  * ex: public interface A extends B, C, D {...}
  * @param modifiers the modifiers for this interface
  * @param name the identifier for this interface
  * @param extensionOf the identifiers of the interfaces this interface extends
  * @param members the field and method declarations in this interface's body
  */
case class InterfaceDecl(modifiers: List[Modifier], name: Identifier, id: Int, extensionOf: List[FullyQualifiedID], members: List[MemberDecl]) extends TypeDecl

/**
  * ClassDecl represents a class declaration
  * ex: public class C extends A implements B {...}
  * @param modifiers the modifiers for this class
  * @param name the identifier for this class
  * @param extensionOf the identifier of the class this class extends
  * @param implementationOf the identifiers of the interfaces this class implements
  * @param members the field, method and constructor declarations in this class' body
  */
case class ClassDecl(modifiers: List[Modifier], name: Identifier, id: Int, extensionOf: Option[FullyQualifiedID], implementationOf: List[FullyQualifiedID], members: List[MemberDecl]) extends TypeDecl

sealed trait MemberDecl extends Decl {

  def modifiers: List[Modifier]

}

/**
  * ConstructorDecl represents a class constructor method in a class or interface body
  * @param modifiers the modifiers for this constructor
  * @param parameters the parameter declarations for this constructor
  * @param body the code body of this constructor that contains statements
  */
case class ConstructorDecl(modifiers: List[Token.Modifier], identifier: Identifier,
                           parameters: List[ParameterDecl], body: BlockStmt) extends MemberDecl

/**
  * Provides a key to find the constructor for overloading
  *
  * @param identifier The identifier of the contructor i.e. the class
  * @param parameters The parameters the constructor takes
  */
case class ConstructorHeader(identifier: Identifier, parameters: List[ParameterDecl])

/**
  * FieldDecl represents a variable declaration within a class body
  * ex: class foo { int bar = 5; }
  * @param modifiers the modifiers for this field
  * @param typ the type of this field
  * @param name the identifier for this field
  * @param assignment an assignment expression to assign to
  */
case class FieldDecl(modifiers: List[Token.Modifier], typ: Type, name: Token.Identifier,
                     assignment: Option[Expr]) extends MemberDecl

/**
  * MethodDecl represents a method declaration
  * ex: int bar() { return 5; }
  * @param modifiers the modifiers for this method
  * @param returnType the return type of this method or None if void return type
  * @param name the identifier for this method
  * @param parameters the parameters for this method
  * @param body the statements in this method
  */
case class MethodDecl(modifiers: List[Token.Modifier], returnType: Option[Type],
                      name: Token.Identifier, parameters: List[ParameterDecl], body: Option[BlockStmt]) extends MemberDecl

/**
  * Provides a key to compare methods for overloading
  *
  * @param name The name of the method
  * @param parameters The paramters of the method
  */
case class MethodHeader(name: Identifier, parameters: List[ParameterDecl])

/**
  * ParameterDecl represents a parameter in a method or constructor
  * @param typ the type of the parameter
  * @param name the identifier of the parameter
  */
case class ParameterDecl(typ: Type, name: Token.Identifier) extends Decl

/**
  * VarDecl represents a variable declaration in a code block
  * @param typ the type of the variable
  * @param name the identifier of the variable
  */
case class VarDecl(typ: Type, name: Token.Identifier) extends Decl
