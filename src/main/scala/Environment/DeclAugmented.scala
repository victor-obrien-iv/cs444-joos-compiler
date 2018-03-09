package Environment

import AST.{FullyQualifiedID, ImportDecl}
import Token.{Identifier, Modifier}

trait AugmentedNode
/**
  * Decl represents a declaration
  */
trait DeclAugmented extends AugmentedNode

/**
  * CompilationUnit is a special declaration that encapsulates everything
  *   in a .java file
  * @param fileName the path and file that is associated with this compilation unit
  * @param packageName the identifier that comes after 'package' in the source code
  *                    or None if no package declaration exists for this file
  * @param imports the list of import declarations in this file
  * @param typeDecl the type declared in the compilation unit
  */
case class CompilationUnitAugmented(fileName: String, packageName: Option[FullyQualifiedID],
                                    imports: List[ImportDecl], typeDecl: TypeDeclAugmented) extends DeclAugmented

/**
  * Represents a Type
  */
sealed trait TypeDeclAugmented extends DeclAugmented {
  def modifiers: List[Token.Modifier]
  def name: Token.Identifier
  def members: List[DeclAugmented]
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
case class InterfaceDeclAugmented(modifiers: List[Token.Modifier], name: Token.Identifier, id: Int,
                                  extensionOf: List[FullyQualifiedID], members: List[DeclAugmented],
                                  environment: Environment) extends TypeDeclAugmented

/**
  * ClassDecl represents a class declaration
  * ex: public class C extends A implements B {...}
  * @param modifiers the modifiers for this class
  * @param name the identifier for this class
  * @param extensionOf the identifier of the class this class extends
  * @param implementationOf the identifiers of the interfaces this class implements
  * @param members the field, method and constructor declarations in this class' body
  */
case class ClassDeclAugmented(modifiers: List[Token.Modifier], name: Token.Identifier, id: Int,
                              extensionOf: Option[FullyQualifiedID], implementationOf: List[FullyQualifiedID],
                              members: List[DeclAugmented],
                              environment: Environment) extends TypeDeclAugmented

/**
  * ConstructorDecl represents a class constructor method in a class or interface body
  * @param modifiers the modifiers for this constructor
  * @param parameters the parameter declarations for this constructor
  * @param body the code body of this constructor that contains statements
  */
case class ConstructorDeclAugmented(modifiers: List[Token.Modifier], identifier: Identifier,
                                    parameters: List[ParameterDeclAugmented], body: BlockStmtAugmented,
                                    environment: Environment) extends DeclAugmented

case class ConstructorHeader(modifiers: List[Modifier], identifier: Identifier,
                             parameters: List[ParameterDeclAugmented])
/**
  * FieldDecl represents a variable declaration within a class body
  * ex: class foo { int bar = 5; }
  * @param modifiers the modifiers for this field
  * @param typ the type of this field
  * @param name the identifier for this field
  * @param assignment an assignment expression to assign to
  */
case class FieldDeclAugmented(modifiers: List[Token.Modifier], typ: TypeAugmented, name: Token.Identifier,
                              assignment: Option[ExprAugmented], environment: Environment) extends DeclAugmented

/**
  * MethodDecl represents a method declaration
  * ex: int bar() { return 5; }
  * @param modifiers the modifiers for this method
  * @param returnType the return type of this method or None if void return type
  * @param name the identifier for this method
  * @param parameters the parameters for this method
  * @param body the statements in this method
  */
case class MethodDeclAugmented(modifiers: List[Token.Modifier], returnType: Option[TypeAugmented],
                               name: Token.Identifier, parameters: List[ParameterDeclAugmented],
                               body: Option[BlockStmtAugmented], environment: Environment) extends DeclAugmented

case class MethodHeader(modifiers: List[Modifier], returnType: Option[TypeAugmented],
                        name: Identifier, paramters: List[ParameterDeclAugmented])

/**
  * ParameterDecl represents a parameter in a method or constructor
  * @param typ the type of the parameter
  * @param name the identifier of the parameter
  */
case class ParameterDeclAugmented(typ: TypeAugmented, name: Token.Identifier,
                                  environment: Environment) extends DeclAugmented

/**
  * VarDecl represents a variable declaration in a code block
  * @param typ the type of the variable
  * @param name the identifier of the variable
  */
case class VarDeclAugmented(typ: TypeAugmented, name: Token.Identifier,
                            environment: Environment) extends DeclAugmented
