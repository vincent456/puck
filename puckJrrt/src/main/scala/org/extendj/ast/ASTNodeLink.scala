package org.extendj.ast

import puck.PuckError
import puck.graph.{TypeDecl => PTypeDecl, _}

object ASTNodeLink{

  def setName(name : String, nl : ASTNodeLink,
              reenactor : DependencyGraph, renamed : NodeId) : Unit = nl match {
    case FieldDeclHolder(decl, idx) => decl.getDeclarator(idx).setID(name)
    case dh : MethodDeclHolder => dh.decl.setID(name)
    case th : TypedKindDeclHolder =>
      val oldName = th.decl.getID
      th.decl.setID(name)
      val cu = th.decl.compilationUnit()
      //TODO if printed in place oldName.java should be deleted
      if(cu.pathName().endsWith(s"$oldName.java"))
        cu.setPathName(cu.pathName().replaceAllLiterally(s"$oldName.java", s"$name.java"))
      import scala.collection.JavaConversions._
      th.decl.constructors().foreach{cdecl =>
        cdecl.flushAttrCache()
        cdecl.setID(name)
      }
      val oldFullName = reenactor.fullName(renamed)
      val newFullName = reenactor.fullName(reenactor.container_!(renamed)) + "." + name
      th.decl.program().changeTypeMap(oldFullName, newFullName, th.decl)

    case ch : ConstructorDeclHolder => ch.decl.setID(name)
    case h => throw new PuckError(h.getClass + " setName unhandled")
  }

  def getPath(graph: DependencyGraph, packagedId : NodeId)
             ( implicit program : Program ) : String = {
    val cpath = graph.containerPath(packagedId)
    val names = cpath.tail.map(graph.getConcreteNode(_).name)
    program.getRootPath + names.mkString(java.io.File.separator)
  }
  def getPath(graph: DependencyGraph, packagedId : NodeId, typeDeclId : NodeId)
             ( implicit program : Program ) : String  =
    getPath(graph, packagedId) + java.io.File.separator +
      graph.getConcreteNode(typeDeclId).name + ".java"


  def enlargeVisibility
  ( g : DependencyGraph,
    astNode : Visible,
    nid : NodeId) : Unit = {

    val needMoreVisibility : NodeId => Boolean =
      g.getConcreteNode(nid).kind.kindType match {
      case PTypeDecl => g.hostNameSpace(_) != g.hostNameSpace(nid)
      case InstanceValueDecl
           | StaticValueDecl => g.hostTypeDecl(_) != g.hostTypeDecl(nid)
      case kt => error(s"$kt not expected")
    }

    import ASTNode.VIS_PUBLIC
    if (astNode.getVisibility != VIS_PUBLIC) {
      if (g .usersOfExcludingTypeUse(nid) exists needMoreVisibility)
        astNode.setVisibility(VIS_PUBLIC)

    }

  }
}

sealed trait ASTNodeLink

case object NoDecl extends ASTNodeLink
sealed abstract class HasNode extends ASTNodeLink {
  def node : ASTNode[_]
}
case object PackageDeclHolder extends ASTNodeLink

sealed abstract class DefHolder extends HasNode
case class ExprHolder(expr : Expr) extends DefHolder{
  def node = expr.asInstanceOf[ASTNode[_]]
}
case class BlockHolder(block : Block) extends DefHolder{
  def node = block.asInstanceOf[ASTNode[_]]
}

case class ParameterDeclHolder(decl : ParameterDeclaration) extends HasNode {
  def node = decl.asInstanceOf[ASTNode[_]]
}

sealed trait HasBodyDecl extends HasNode{
  val decl : BodyDecl
  def node = decl.asInstanceOf[ASTNode[_]]
}

sealed trait HasMemberDecl extends HasBodyDecl{
  override val decl : MemberDecl
}

object VariableDeclHolder {
  def unapply(nl : ASTNodeLink) : Option[Variable] = nl match {
    case FieldDeclHolder(decl, idx) => Some(decl.getDeclarator(idx))
    case ParameterDeclHolder(decl) => Some(decl)
    case _ => None
  }

}

class DeclarationCreationError(msg : String) extends DGError(msg)

case class ConstructorDeclHolder(decl : ConstructorDecl) extends HasBodyDecl
case class MethodDeclHolder(decl : MethodDecl) extends HasMemberDecl

object CallableDeclHolder {
  def unapply(nl : ASTNodeLink) : Option[Callable] = nl match {
    case ConstructorDeclHolder(cdecl) => Some(cdecl)
    case MethodDeclHolder(mdecl) => Some(mdecl)
    case _ => None
  }
}

case class FieldDeclHolder(decl : FieldDecl, declaratorIndex : Int) extends HasMemberDecl {
  def declarator = decl.getDeclarator(declaratorIndex)
}

case class EnumConstantHolder(decl : EnumConstant) extends HasBodyDecl

trait TypedKindDeclHolder extends HasNode {
  def decl : TypeDecl
  def node = decl.asInstanceOf[ASTNode[_]]
}


case class InterfaceDeclHolder(decl : InterfaceDecl) extends TypedKindDeclHolder
case class ClassDeclHolder(decl : ClassDecl) extends TypedKindDeclHolder
case class WildCardTypeHolder(decl : AbstractWildcardType) extends TypedKindDeclHolder
case class TypeVariableHolder(decl : TypeVariable) extends TypedKindDeclHolder
case class PrimitiveDeclHolder(decl : TypeDecl) extends TypedKindDeclHolder