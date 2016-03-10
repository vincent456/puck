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

package puck.intellij.graphBuilding

import com.intellij.psi._
import puck.graph._

/**
 * Created by Loïc Girault on 28/10/15.
 */
object TypeUserFromQualifier extends JavaElementVisitor {
  var typeUser : Option[NodeId] = None
  var builder : IntellijGraphBuilder = null

  def setTypeUser(psiElement: PsiElement) : Unit = {
    typeUser = Some(GetNode(psiElement)(builder))
  }

  def apply
  ( psiElement: PsiElement)
  ( implicit builder: IntellijGraphBuilder ) : NodeId = {
    typeUser = None
    this.builder = builder
    psiElement.accept(this)
    typeUser getOrElse error(s"type user not found for $psiElement in ${psiElement.getContainingFile.getName}")
  }

//  override def visitMethodReferenceExpression(expression: PsiMethodReferenceExpression): Unit =
//    typeUser = Some(GetNode(expression.resolve()))

  override def visitArrayAccessExpression(expression: PsiArrayAccessExpression) : Unit = {
    expression.getArrayExpression.accept(this)
  }

  override def visitMethodCallExpression(expression: PsiMethodCallExpression) : Unit =
    setTypeUser(expression.getMethodExpression.resolve())


  override def visitThisExpression(expression: PsiThisExpression): Unit ={
    Option(expression.getQualifier) match {
      case None => Utils.psiClassAncestor(expression) foreach setTypeUser
      case Some(q) => sys.error(s"this qualified by $q TypeUserFromQualifier case unhandled")
    }

  }

  override def visitReferenceExpression(reference: PsiReferenceExpression) : Unit =
    setTypeUser(reference.resolve())




}
