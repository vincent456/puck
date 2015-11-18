package puck.intellij

import com.intellij.psi._
import puck.javaGraph.nodeKind._


sealed trait PsiNodeWrapper{
  def kind : JavaNodeKind
}

//case object NoDecl extends PsiNodeWrapper
sealed abstract class HasNode extends PsiNodeWrapper {
  def node : PsiElement
}
case object PackageDummyWrapper extends PsiNodeWrapper {
  def kind  = Package
}
case object EmptyDef extends PsiNodeWrapper {
  def kind = Definition
}

sealed abstract class DefHolder extends HasNode {
  def kind = Definition
}
case class ExprHolder(expr : PsiExpression) extends DefHolder{
  def node = expr
}
case class BlockHolder(block : PsiCodeBlock) extends DefHolder{
  def node = block
}

case class ParameterDeclHolder(decl : PsiParameter) extends HasNode {
  def kind = Param
  def node = decl
}

sealed abstract class MemberDeclHolder extends HasNode {
  def node : PsiMember
}
case class FieldDeclHolder(node : PsiField) extends MemberDeclHolder {
  def kind = Field
}
case class ConstructorDeclHolder(node : PsiMethod) extends MemberDeclHolder{
  def kind = Constructor
}
case class MethodDeclHolder(node : PsiMethod) extends MemberDeclHolder{
  def kind = Method
}

//sealed trait HasBodyDecl extends HasNode{
//  val decl : PsiCodeBlock
//  def node = decl
//}
//
//sealed trait HasMemberDecl extends HasBodyDecl{
//  override val decl : PsiMember
//}
//
//object VariableDeclHolder {
//  def unapply(nl : ASTNodeLink) : Option[PsiVariable] = nl match {
//    case FieldDeclHolder(decl) => Some(decl)
//    case ParameterDeclHolder(decl) => Some(decl)
//    case _ => None
//  }
//
//}
//
//class DeclarationCreationError(msg : String) extends DGError(msg)
//
//case class ConstructorDeclHolder(decl : AST.ConstructorDecl) extends HasBodyDecl
//
//case class FieldDeclHolder(decl : PsiField) extends HasMemberDecl
//
//object MethodDeclHolder {
//  def unapply(mdh : MethodDeclHolder) : Some[AST.MethodDecl] =
//    Some(mdh.decl)
//}
//
//trait MethodDeclHolder extends HasMemberDecl {
//  val decl : AST.MethodDecl
//}
//
//case class ConcreteMethodDeclHolder(decl : PsiMethod) extends MethodDeclHolder
//
//case class AbstractMethodDeclHolder(decl : PsiMethod) extends MethodDeclHolder

sealed abstract class TypedKindDeclHolder extends HasNode {
  def node : PsiClass
}


case class InterfaceDeclHolder(node : PsiClass) extends TypedKindDeclHolder{
  def kind = Interface
}
case class ClassDeclHolder(node : PsiClass) extends TypedKindDeclHolder{
  def kind = Class
}
//case class WildCardTypeHolder(decl : AST.WildcardType) extends TypedKindDeclHolder
//case class TypeVariableHolder(decl : AST.TypeVariable) extends TypedKindDeclHolder
//TODO : redo properly
case object ArrayTypeWrapper extends PsiNodeWrapper {
  def kind = Primitive
}
case class PrimitiveDeclHolder(node : PsiPrimitiveType) extends PsiNodeWrapper {
  def kind = Primitive
}