/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package org.extendj.ast

import puck.graph.{TypeDecl => PTypeDecl, _}
import puck.jastadd.concretize.RedirectSource
import puck.util.PuckLogger

object ASTNodeLink{

  def setName(name : String, dg2ast : NodeId => ASTNodeLink,
              reenactor : DependencyGraph, renamed : NodeId)
             ( implicit logger : PuckLogger) : Unit = dg2ast(renamed) match {
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
      val containerFullName = reenactor.fullName(reenactor.container_!(renamed))

      val newFullName =
        if(containerFullName.isEmpty) name
        else  s"$containerFullName.$name"
      th.decl.program().changeTypeMap(oldFullName, newFullName, th.decl)

    case ch : ConstructorDeclHolder => ch.decl.setID(name)
    case h => setPackageName(name, dg2ast, reenactor, renamed)
  }

  def setPackageName(name : String, dg2ast : NodeId => ASTNodeLink,
                     reenactor : DependencyGraph, renamed : NodeId)
                    ( implicit logger : PuckLogger) : Unit = {

    val oldName = reenactor fullName renamed
    val newName = {
      val containerName = reenactor fullName (reenactor container_! renamed)
      if(containerName.isEmpty) name
      else s"$containerName.$name"
    }

    reenactor.content(renamed) map (id => (id, dg2ast(id))) foreach {
        case (tid, TypedKindDeclHolder(td)) =>
          td.compilationUnit().setPackageDecl(newName) //both setter and affectation needed
          td.compilationUnit().packageName_value = newName //this value is the cached one
          RedirectSource.fixImportForUsersOfMovedTypeDecl(reenactor, dg2ast, td, tid, oldName, newName)
          RedirectSource.fixImportOfMovedTypeDecl(reenactor, dg2ast, td, tid, oldName, newName, newlyCreatedCu = false)
      }
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
//object ASTNodeHolder {
//  def unapply(nl : ASTNodeLink) : Option[ASTNode[_]] = nl match {
//    case hn : HasNode => Some(hn.node)
//    case _ => None
//  }
//}
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

object TypedKindDeclHolder {
  def unapply(l : ASTNodeLink) : Option[TypeDecl] =
    l match {
      case th : TypedKindDeclHolder => Some(th.decl)
      case _ => None
    }
}

case class InterfaceDeclHolder(decl : InterfaceDecl) extends TypedKindDeclHolder
case class ClassDeclHolder(decl : ClassDecl) extends TypedKindDeclHolder
case class WildCardTypeHolder(decl : AbstractWildcardType) extends TypedKindDeclHolder
case class TypeVariableHolder(decl : TypeVariable) extends TypedKindDeclHolder
case class PrimitiveDeclHolder(decl : TypeDecl) extends TypedKindDeclHolder