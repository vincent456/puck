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

import puck.graph.{Contains, DGBuildingError, NodeId}
import puck.javaGraph.nodeKind.{TypeVariable => PTypeVariable, EnumConstant => PEnumConstant, _}
/**
  * Created by Loïc Girault on 19/05/16.
  */
trait NodeFactory {
  this : JastaddGraphBuilder =>

  def getDefinition(bd : DGNamedElement) = {
    val n = g.getConcreteNode(buildNode(bd))
    n.definition(g).getOrElse {
      val defNode = getDefNode(bd)
      g = g.addEdge(Contains(n.id, defNode))
      defNode
    }
  }

  def buildNode( a : Access) : NodeId = a match {
    case va : VarAccess if va.decl().isLocalVariable => // No lock
      getDefinition(va.hostBodyDecl.getDGNamedNode)

    case cie: ClassInstanceExpr =>
      if(!cie.hasTypeDecl)
        cie.lock()
      buildNode(cie.accessed().asInstanceOf[DGNamedElement])
    case d : Dot => buildNode(d.getRight)

    case ac : Access =>
      if(!ac.isSubstitute /*&& !ac.accessed().isInstanceOf[TypeVariable]*/)
        ac.lock()
      buildNode(ac.accessed().asInstanceOf[DGNamedElement])
  }

  def checkSubstituteAndBuild(n : DGNamedElement) : NodeId =
    if( n.isSubstitute )
      getNode(n.asInstanceOf[Substitute].getOriginal.asInstanceOf[DGNamedElement])
    else getNode( n )

  def buildNode(n : DGNamedElement) : NodeId = n match {
    case ad : ArrayDecl => arrayTypeId
    case ptd : ParTypeDecl => buildNode(ptd.genericDecl)
    case wc : AbstractWildcardType if wc.isNamedElement =>
      getNode(wc)
    case _ => checkSubstituteAndBuild(n)

  }


  def buildNode(pd : ParameterDeclaration) : NodeId =
    if (pd.getParent.isInstanceOf[CatchClause]) buildNode(pd.hostBodyDecl())
    else getNode(pd)

  def buildNode(methodDecl: MethodDecl) : NodeId =
    checkSubstituteAndBuild(methodDecl)
  def buildNode(constructorDecl: ConstructorDecl) : NodeId =
    checkSubstituteAndBuild(constructorDecl)


  def buildNode(bodyDecl: BodyDecl) : NodeId = bodyDecl match {
    case n : DGNamedElement => buildNode(n.asInstanceOf[DGNamedElement])
    case fd : FieldDecl =>
      if(fd.getNumDeclarator == 1) buildNode(fd.getDeclarator(0))
      else throw new DGBuildingError(s"FieldDecl with ${fd.getNumDeclarator} declarators not expected")
    case _ : StaticInitializer | _ : InstanceInitializer => buildNode(bodyDecl.hostType())
    case _ => throw new DGBuildingError(bodyDecl + " not expected")
  }

  def buildNode(vd : VariableDeclarator) : NodeId = {

    def isAnonymousBlock(bd : BodyDecl) =
      bd.isInstanceOf[StaticInitializer] || bd.isInstanceOf[InstanceInitializer]

    val n = buildNode(vd.hostBodyDecl())
    if(isAnonymousBlock(vd.hostBodyDecl()))  n
    else definitionOf(n).get
  }

  def buildNode(n : Expr) : NodeId = n match {
    case a : Access => buildNode(a)
    case l : Literal => buildNode(l.`type`())
    case pexpr : ParExpr => buildNode(pexpr.getExpr)
    case expr : Expr => definitionOf(buildNode(expr.hostBodyDecl())).get
    case _ => throw new DGBuildingError()
  }

  def refTypeNodeKind(n : ReferenceType) : JavaNodeKind = {

    def aux(n : ReferenceType) : TypeKind = n match {
      case gcd : GenericClassDecl => GenericClass
      case cd : ClassDecl => Class
      case gid : GenericInterfaceDecl => GenericInterface
      case id : InterfaceDecl => Interface
      case _ =>
        throw new DGBuildingError(s"${n.dgFullName()} - ${n.getClass} : Unkown nodekind")
    }

    if(n.isInnerType) Inner(aux(n))
    else aux(n)
  }

  def nodeKind(n : DGNamedElement) : JavaNodeKind = n match {
    case _ : TypeVariable => PTypeVariable
    case rt : ReferenceType => refTypeNodeKind(rt)
    case _ : AbstractWildcardType => WildCardType
    case _ : PrimitiveType
         | _ : VoidType
         | _ : NullType => Primitive
    case _ : ParameterDeclaration => Param
    case _ : EnumConstant => PEnumConstant
    case fd : FieldDeclarator =>
      if(fd.isStatic) StaticField
      else Field
    case md : MethodDecl =>
      if(md.isAbstract) AbstractMethod
      else if (md.isStatic) StaticMethod
      else Method
    case _ : ConstructorDecl => Constructor
    case _ =>
      throw new DGBuildingError(s"${n.dgFullName()} - ${n.getClass} : Unkown nodekind")
  }
}
