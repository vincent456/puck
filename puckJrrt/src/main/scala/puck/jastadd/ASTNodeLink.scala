package puck.jastadd

import puck.PuckError
import puck.graph._
import org.extendj.ast

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
             ( implicit program : ast.Program ) : String = {
    val cpath = graph.containerPath(packagedId)
    val names = cpath.tail.map(graph.getConcreteNode(_).name)
    program.getRootPath + names.mkString(java.io.File.separator)
  }
  def getPath(graph: DependencyGraph, packagedId : NodeId, typeDeclId : NodeId)
             ( implicit program : ast.Program ) : String  =
    getPath(graph, packagedId) + java.io.File.separator +
      graph.getConcreteNode(typeDeclId).name + ".java"


  def enlargeVisibility
  ( g : DependencyGraph,
    astNode : ast.Visible,
    nid : NodeId) : Unit = {

    val needMoreVisibility : NodeId => Boolean =
      g.getConcreteNode(nid).kind.kindType match {
      case TypeDecl => g.hostNameSpace(_) != g.hostNameSpace(nid)
      case InstanceValueDecl
           | StaticValueDecl => g.hostTypeDecl(_) != g.hostTypeDecl(nid)
      case kt => error(s"$kt not expected")
    }

    import ast.ASTNode.VIS_PUBLIC
    if (astNode.getVisibility != VIS_PUBLIC) {
      if (g .usersOfExcludingTypeUse(nid) exists needMoreVisibility)
        astNode.setVisibility(VIS_PUBLIC)

    }

  }
}

sealed trait ASTNodeLink

case object NoDecl extends ASTNodeLink
sealed abstract class HasNode extends ASTNodeLink {
  def node : ast.ASTNode[_]
}
case object PackageDeclHolder extends ASTNodeLink

sealed abstract class DefHolder extends HasNode
case class ExprHolder(expr : ast.Expr) extends DefHolder{
  def node = expr.asInstanceOf[ast.ASTNode[_]]
}
case class BlockHolder(block : ast.Block) extends DefHolder{
  def node = block.asInstanceOf[ast.ASTNode[_]]
}

case class ParameterDeclHolder(decl : ast.ParameterDeclaration) extends HasNode {
  def node = decl.asInstanceOf[ast.ASTNode[_]]
}

sealed trait HasBodyDecl extends HasNode{
  val decl : ast.BodyDecl
  def node = decl.asInstanceOf[ast.ASTNode[_]]
}

sealed trait HasMemberDecl extends HasBodyDecl{
  override val decl : ast.MemberDecl
}

object VariableDeclHolder {
  def unapply(nl : ASTNodeLink) : Option[ast.Variable] = nl match {
    case FieldDeclHolder(decl, idx) => Some(decl.getDeclarator(idx))
    case ParameterDeclHolder(decl) => Some(decl)
    case _ => None
  }

}

class DeclarationCreationError(msg : String) extends DGError(msg)

case class ConstructorDeclHolder(decl : ast.ConstructorDecl) extends HasBodyDecl
case class MethodDeclHolder(decl : ast.MethodDecl) extends HasMemberDecl

object CallableDeclHolder {
  def unapply(nl : ASTNodeLink) : Option[ast.Callable] = nl match {
    case ConstructorDeclHolder(cdecl) => Some(cdecl)
    case MethodDeclHolder(mdecl) => Some(mdecl)
    case _ => None
  }
}

case class FieldDeclHolder(decl : ast.FieldDecl, declaratorIndex : Int) extends HasMemberDecl {
  def declarator = decl.getDeclarator(declaratorIndex)
}

case class EnumConstantHolder(decl : ast.EnumConstant) extends HasBodyDecl

trait TypedKindDeclHolder extends HasNode {
  def decl : ast.TypeDecl
  def node = decl.asInstanceOf[ast.ASTNode[_]]
}


case class InterfaceDeclHolder(decl : ast.InterfaceDecl) extends TypedKindDeclHolder
case class ClassDeclHolder(decl : ast.ClassDecl) extends TypedKindDeclHolder
case class WildCardTypeHolder(decl : ast.WildcardType) extends TypedKindDeclHolder
case class TypeVariableHolder(decl : ast.TypeVariable) extends TypedKindDeclHolder
case class PrimitiveDeclHolder(decl : ast.TypeDecl) extends TypedKindDeclHolder