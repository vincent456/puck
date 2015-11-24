package puck.jastadd

import puck.PuckError
import puck.graph._
import org.extendj.{ast => AST}

object ASTNodeLink{

  def setName(name : String, nl : ASTNodeLink) : Unit = nl match {
    case FieldDeclHolder(decl) => decl.setID(name)
    case dh : MethodDeclHolder => dh.decl.setID(name)
    case th : TypedKindDeclHolder =>
      val cu = th.decl.compilationUnit()
      if(cu.getID == th.decl.getID)
        cu.setID(name)
      th.decl.setID(name)
    case ch : ConstructorDeclHolder => ch.decl.setID(name)
    case h => throw new PuckError(h.getClass + " setName unhandled")
  }

  def getPath(graph: DependencyGraph, typeDeclId : NodeId)
             ( implicit program : AST.Program ) = {
    val cpath = graph.containerPath(typeDeclId)
    val names = cpath.tail.map(graph.getConcreteNode(_).name)
    program.getRootPath + names.mkString(java.io.File.separator) +".java"
  }

  def enlargeVisibility
  ( g : DependencyGraph,
    astNode : AST.Visible,
    nid : NodeId) : Unit = {
    val needMoreVisibility : NodeId => Boolean =
      g.getConcreteNode(nid).kind.kindType match {
      case TypeDecl => g.hostNameSpace(_) != g.hostNameSpace(nid)
      case InstanceValueDecl
           | StaticValueDecl => g.hostTypeDecl(_) != g.hostTypeDecl(nid)
    }
    import AST.ASTNode.VIS_PUBLIC
    if (astNode.getVisibility != VIS_PUBLIC) {
      if (g .usersOf(nid) exists needMoreVisibility)
        astNode.setVisibility(VIS_PUBLIC)

    }

  }
}

sealed trait ASTNodeLink

case object NoDecl extends ASTNodeLink
sealed abstract class HasNode extends ASTNodeLink {
  def node : AST.ASTNode[_]
}
case object PackageDeclHolder extends ASTNodeLink

sealed abstract class DefHolder extends HasNode
case class ExprHolder(expr : AST.Expr) extends DefHolder{
  def node = expr.asInstanceOf[AST.ASTNode[_]]
}
case class BlockHolder(block : AST.Block) extends DefHolder{
  def node = block.asInstanceOf[AST.ASTNode[_]]
}

case class ParameterDeclHolder(decl : AST.ParameterDeclaration) extends HasNode {
  def node = decl.asInstanceOf[AST.ASTNode[_]]
}

sealed trait HasBodyDecl extends HasNode{
  val decl : AST.BodyDecl
  def node = decl.asInstanceOf[AST.ASTNode[_]]
}

sealed trait HasMemberDecl extends HasBodyDecl{
  override val decl : AST.MemberDecl
}

object VariableDeclHolder {
  def unapply(nl : ASTNodeLink) : Option[AST.Variable] = nl match {
    case FieldDeclHolder(decl) => Some(decl)
    case ParameterDeclHolder(decl) => Some(decl)
    case _ => None
  }

}

class DeclarationCreationError(msg : String) extends DGError(msg)

case class ConstructorDeclHolder(decl : AST.ConstructorDecl) extends HasBodyDecl

case class FieldDeclHolder(decl : AST.FieldDeclaration) extends HasMemberDecl

object MethodDeclHolder {
  def unapply(mdh : MethodDeclHolder) : Some[AST.MethodDecl] =
    Some(mdh.decl)
}

trait MethodDeclHolder extends HasMemberDecl {
  val decl : AST.MethodDecl
}

case class ConcreteMethodDeclHolder(decl : AST.MethodDecl) extends MethodDeclHolder

case class AbstractMethodDeclHolder(decl : AST.MethodDecl) extends MethodDeclHolder

trait TypedKindDeclHolder extends HasNode {
  def decl : AST.TypeDecl
  def node = decl.asInstanceOf[AST.ASTNode[_]]
}


case class InterfaceDeclHolder(decl : AST.InterfaceDecl) extends TypedKindDeclHolder
case class ClassDeclHolder(decl : AST.ClassDecl) extends TypedKindDeclHolder
case class WildCardTypeHolder(decl : AST.WildcardType) extends TypedKindDeclHolder
case class TypeVariableHolder(decl : AST.TypeVariable) extends TypedKindDeclHolder
case class PrimitiveDeclHolder(decl : AST.TypeDecl) extends TypedKindDeclHolder