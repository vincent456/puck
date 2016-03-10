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
import com.intellij.psi.impl.source.PsiMethodImpl
import com.intellij.psi.util.PsiTreeUtil
import puck.graph.{DependencyGraph, NodeId}
import puck.intellij._

/**
 * Created by Loïc Girault on 22/10/15.
 */
object GetNode {

  def apply
  ( qualifiedName : String,
    name : String,
    wrapper : PsiNodeWrapper,
    mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder ) : NodeId = {

    val node = builder.addNode(qualifiedName, name, wrapper.kind, mutable)
    builder.graph2ASTMap get node match {
      case None => builder.graph2ASTMap += (node -> wrapper)
      case Some(_) => ()
    }
    node
  }

  def anonymous
  ( qualif : String, wrapper : PsiNodeWrapper, mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder )=
    apply(qualif + "." + DependencyGraph.anonymousName,
      DependencyGraph.anonymousName, wrapper, mutable)

  def parameter
  ( qualif : String, param: PsiParameter, mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
  apply(qualif+"."+ param.getName, param.getName, ParameterDeclHolder(param), mutable)

  def apply
  ( psiElement: PsiElement)
  ( implicit builder: IntellijGraphBuilder ) : NodeId = {
    val mutable = builder.fromSource(psiElement)
    psiElement match {
      case c: PsiClass => apply(c, mutable)
      case f: PsiField => apply(f, mutable)
      case m: PsiMethod => apply(m, mutable)
      case p: PsiParameter => apply(p, mutable)
      case v: PsiLocalVariable => apply(v, mutable)
      case p: PsiPackage => apply(p, mutable)
      //    case e : PsiExpression => apply(e, mutable)
      //    case b : PsiCodeBlock => apply(b, mutable)
      case _ =>
        sys.error(s"GetNode(t : $psiElement) unhandled case")
    }
  }

  def apply
  ( c : PsiClass, mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
    apply(c.getQualifiedName, c.getName, wrapClass(c), mutable)

  def wrapClass(aClass: PsiClass) : PsiNodeWrapper =
    if(aClass.isInterface) InterfaceDeclHolder(aClass)
    else ClassDeclHolder(aClass)

  def apply
  ( method: PsiMethod, mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
    apply(QualifiedName(method), method.getName, MethodDeclHolder(method), mutable)

  def apply
  ( field: PsiField, mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
    apply(QualifiedName(field), field.getName, FieldDeclHolder(field), mutable)



  def apply
  ( param: PsiParameter, mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder ) : NodeId = {
    param.getDeclarationScope match {
      case m : PsiMember => parameter(QualifiedName.memberQN(m), param, mutable)
      case s : PsiForeachStatement => psiMemberDefAncestorNode(s, mutable)
      case c : PsiCatchSection => psiMemberDefAncestorNode(c, mutable)
      case p => sys.error(s"$p unexpected parameter parent in " + p.getContainingFile +" - "+ p.getContext)
    }
  }

  def apply
  ( p : PsiPackage, mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
   builder.addPackage(p.getQualifiedName, mutable = true)



  def psiMemberDefAncestorNode
  (elt : PsiElement, mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder ): NodeId =
    Utils.psiMemberAncestor(elt.getParent) match {
    case None => sys.error(elt + "memberAncestor not found")
    case Some(m : PsiMethod) =>
      anonymous(QualifiedName(m), BlockHolder(m.getBody), mutable)
    case Some(f : PsiField) =>
      anonymous(QualifiedName(f), ExprHolder(f.getInitializer), mutable)
    case Some(m : PsiMember) =>
          sys.error("member kind not handled")
  }

  def apply
  ( lvar: PsiLocalVariable, mutable : Boolean)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
    psiMemberDefAncestorNode(lvar, mutable)

//  def apply
//  ( e: PsiExpression)
//  ( implicit builder: IntellijGraphBuilder ) =
//
//  def apply
//  ( b: PsiCodeBlock)
//  ( implicit builder: IntellijGraphBuilder ) =

  def apply
  ( t : PsiType)
  ( implicit builder: IntellijGraphBuilder ) : NodeId = t match {
    case pct : PsiClassType => GetNode(pct.resolve(), builder.fromSource(pct.resolve()))
    case ppt : PsiPrimitiveType =>
      val name = ppt.getCanonicalText(false)
      apply(s"$primitivePackage.$name", name, PrimitiveDeclHolder(ppt), mutable = false)
    case pat : PsiArrayType =>
      val name = Name(t)
      apply(s"$primitivePackage.$name", name, ArrayTypeWrapper, mutable = false)
    case _ =>
      sys.error(s"IntellijGraphBuilder.getTypeNode(t : ${t.getClass}) unhandled case")
  }
}
