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

import puck.graph.{DGBuildingError, NodeId}

/**
  * Created by Loïc Girault on 19/05/16.
  */
trait NodeFactory {
  this : JastaddGraphBuilder =>

  def buildNode( a : Access) : NodeId = a match {
    case va : VarAccess if va.decl().isLocalVariable => // No lock
      getDefinition(buildNode(va.hostBodyDecl()))
    case cie: ClassInstanceExpr =>
      if(!cie.hasTypeDecl)
        cie.lock()
      buildNode(cie.accessed())
    case d : Dot =>
      if(d.isRightRotated)
        d.rotateLeft()
      buildNode(d.getRight)
    case ac : Access =>
      if(!ac.isSubstitute)
        ac.lock()
      buildNode(ac.accessed())
  }

  def buildNode(n : DGNamedElement) : NodeId = n match {
    case ad : ArrayDecl => arrayTypeId
    case ptd : ParTypeDecl => buildNode(ptd.genericDecl)
    case wc : AbstractWildcardType if wc.isNamedElement =>
      getNode(wc)
    case _ =>
      if( n.isSubstitute )
        getNode(n.asInstanceOf[Substitute].getOriginal.asInstanceOf[DGNamedElement])
      else getNode( n )
  }


  def buildNode(pd : ParameterDeclaration) : NodeId =
    if (pd.getParent.isInstanceOf[CatchClause]) buildNode(pd.hostBodyDecl())
    else getNode(pd)

  def buildNode(bodyDecl: BodyDecl) : NodeId = bodyDecl match {
    case n : DGNamedElement => buildNode(n.asInstanceOf[DGNamedElement])
    case fd : FieldDecl =>
      if(fd.getNumDeclarator == 1) buildNode(fd.getDeclarator(0))
      else throw new DGBuildingError(s"FieldDecl with ${fd.getNumDeclarator} declarators not expected")
    case _ => throw new DGBuildingError(bodyDecl + " not expected")
  }

  def buildNode(vd : VariableDeclarator) : NodeId = {

    def isAnonymousBlock(bd : BodyDecl) =
      bd.isInstanceOf[StaticInitializer] || bd.isInstanceOf[InstanceInitializer]

    val n = buildNode(vd.hostBodyDecl())
    if(isAnonymousBlock(vd.hostBodyDecl()))  n
    else getDefinition(n)
  }

  def buildNode(n : Expr) : NodeId = n match {
    case a : Access => buildNode(a)
    case l : Literal => buildNode(l.`type`())
    case pexpr : ParExpr => buildNode(pexpr.getExpr)
    case expr : Expr => getDefinition(buildNode(expr.hostBodyDecl()))
    case _ => throw new DGBuildingError()
  }
}
