package AST

import Token.Identifier

/**
  * Decl represents a declaration
  */
trait Decl extends AstNode

/**
  * CompilationUnit is a special declaration that encapsulates everything
  *   in a .java file
  * @param packageName the identifier that comes after 'package' in the source code
  *                    or None if no package declaration exists for this file
  * @param imports the list of import declarations in this file
  * @param interfaces the list of interface declarations in this file
  * @param classes the list of class declarations in this file
  */
case class CompilationUnit(packageName: Option[FullyQualifiedID], imports: List[ImportDecl],
                           interfaces: List[InterfaceDecl], classes: List[ClassDecl]) extends Decl

/**
  * ImportDecl represents an 'import' usage in the source code
  * @param name the identifier for the import
  * @param asterisk if the import had the asterisk wildcard at the end of it
  */
case class ImportDecl(name: FullyQualifiedID, asterisk : Boolean) extends Decl

/**
  * InterfaceDecl represents an interface declaration
  * ex: public interface A extends B, C, D {...}
  * @param modifiers the modifiers for this interface
  * @param name the identifier for this interface
  * @param extensionOf the identifiers of the interfaces this interface extends
  * @param members the field and method declarations in this interface's body
  */
case class InterfaceDecl(modifiers: List[Token.Modifier], name: Token.Identifier,
                         extensionOf: List[FullyQualifiedID], members: List[Decl]) extends Decl

/**
  * ClassDecl represents a class declaration
  * ex: public class C extends A implements B {...}
  * @param modifiers the modifiers for this class
  * @param name the identifier for this class
  * @param extensionOf the identifier of the class this class extends
  * @param implementationOf the identifiers of the interfaces this class implements
  * @param members the field, method and constructor declarations in this class' body
  */
case class ClassDecl(modifiers: List[Token.Modifier], name: Token.Identifier, extensionOf: Option[FullyQualifiedID],
                     implementationOf: List[FullyQualifiedID], members: List[Decl]) extends Decl

/**
  * ConstructorDecl represents a class constructor method in a class or interface body
  * @param modifiers the modifiers for this constructor
  * @param parameters the parameter declarations for this constructor
  * @param body the code body of this constructor that contains statements
  */
case class ConstructorDecl(modifiers: List[Token.Modifier], identifier: Identifier,
                           parameters: List[ParameterDecl], body: BlockStmt) extends Decl

/**
  * FieldDecl represents a variable declaration within a class body
  * ex: class foo { int bar = 5; }
  * @param modifiers the modifiers for this field
  * @param typ the type of this field
  * @param name the identifier for this field
  * @param assignment an assignment expression to assign to
  */
case class FieldDecl(modifiers: List[Token.Modifier], typ: Type, name: Token.Identifier,
                     assignment: Option[Expr]) extends Decl

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
                      name: Token.Identifier, parameters: List[ParameterDecl], body: BlockStmt) extends Decl

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
