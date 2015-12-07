package puck.jastadd

import puck.PuckError
import puck.graph._
import org.extendj.{ast => AST}

object ASTNodeLink{

  def setName(name : String, nl : ASTNodeLink,
              reenactor : DependencyGraph, renamed : NodeId) : Unit = nl match {
    case FieldDeclHolder(decl) => decl.setID(name)
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
             ( implicit program : AST.Program ) : String = {
    val cpath = graph.containerPath(packagedId)
    val names = cpath.tail.map(graph.getConcreteNode(_).name)
    program.getRootPath + names.mkString(java.io.File.separator)
  }
  def getPath(graph: DependencyGraph, packagedId : NodeId, typeDeclId : NodeId)
             ( implicit program : AST.Program ) : String  =
    getPath(graph, packagedId) + java.io.File.separator +
      graph.getConcreteNode(typeDeclId).name + ".java"


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
case class MethodDeclHolder(decl : AST.MethodDecl) extends HasMemberDecl

object CallableDeclHolder {
  def unapply(nl : ASTNodeLink) : Option[AST.Callable] = nl match {
    case ConstructorDeclHolder(cdecl) => Some(cdecl)
    case MethodDeclHolder(mdecl) => Some(mdecl)
    case _ => None
  }
}

case class FieldDeclHolder(decl : AST.FieldDeclaration) extends HasMemberDecl


trait TypedKindDeclHolder extends HasNode {
  def decl : AST.TypeDecl
  def node = decl.asInstanceOf[AST.ASTNode[_]]
}


case class InterfaceDeclHolder(decl : AST.InterfaceDecl) extends TypedKindDeclHolder
case class ClassDeclHolder(decl : AST.ClassDecl) extends TypedKindDeclHolder
case class WildCardTypeHolder(decl : AST.WildcardType) extends TypedKindDeclHolder
case class TypeVariableHolder(decl : AST.TypeVariable) extends TypedKindDeclHolder
case class PrimitiveDeclHolder(decl : AST.TypeDecl) extends TypedKindDeclHolder