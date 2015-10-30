package puck.intellij.graphBuilding

import com.intellij.psi._
import com.intellij.psi.impl.source.PsiMethodImpl
import com.intellij.psi.util.PsiTreeUtil
import puck.graph.{DependencyGraph, NodeId}
import puck.intellij._

/**
 * Created by lorilan on 22/10/15.
 */
object GetNode {

  def apply
  ( qualifiedName : String,
    name : String,
    wrapper : PsiNodeWrapper,
    mutable : Boolean = true)
  ( implicit builder: IntellijGraphBuilder ) : NodeId = {
    val node = builder.addNode(qualifiedName, name, wrapper.kind, mutable)
    builder.graph2ASTMap get node match {
      case None => builder.graph2ASTMap += (node -> wrapper)
      case Some(_) => ()
    }
    node
  }

  def anonymous
  ( qualif : String, wrapper : PsiNodeWrapper)
  ( implicit builder: IntellijGraphBuilder )=
    apply(qualif + "." + DependencyGraph.anonymousName,
      DependencyGraph.anonymousName, wrapper)

  def parameter
  ( qualif : String, param: PsiParameter)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
  apply(qualif+"."+ param.getName, param.getName, ParameterDeclHolder(param))

  def apply
  ( psiElement: PsiElement)
  ( implicit builder: IntellijGraphBuilder ) : NodeId = psiElement match {
    case c : PsiClass => apply(c)
    case f : PsiField => apply(f)
    case m : PsiMethod => apply(m)
    case p : PsiParameter => apply(p)
    case v : PsiLocalVariable => apply(v)
    case p : PsiPackage => apply(p)
//    case e : PsiExpression => apply(e)
//    case b : PsiCodeBlock => apply(b)
    case _ =>
      sys.error(s"GetNode(t : $psiElement) unhandled case")
  }

  def apply
  ( c : PsiClass)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
    apply(c.getQualifiedName, c.getName, wrapClass(c))

  def wrapClass(aClass: PsiClass) : PsiNodeWrapper =
    if(aClass.isInterface) InterfaceDeclHolder(aClass)
    else ClassDeclHolder(aClass)

  def apply
  ( method: PsiMethod )
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
    apply(QualifiedName(method), method.getName, MethodDeclHolder(method))

  def apply
  ( field: PsiField)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
    apply(QualifiedName(field), field.getName, FieldDeclHolder(field))



  def apply
  ( param: PsiParameter)
  ( implicit builder: IntellijGraphBuilder ) : NodeId = {
    param.getDeclarationScope match {
      case m : PsiMember => parameter(QualifiedName.memberQN(m), param)
      case s : PsiForeachStatement => psiMemberDefAncestorNode(s)
      case c : PsiCatchSection => psiMemberDefAncestorNode(c)
      case p => sys.error(s"$p unexpected parameter parent in " + p.getContainingFile +" - "+ p.getContext)
    }
  }

  def apply
  ( p : PsiPackage)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
    builder.addPackage(p.getQualifiedName, mutable = true)


  def psiMemberDefAncestorNode
  (elt : PsiElement)
  ( implicit builder: IntellijGraphBuilder ): NodeId =
    Utils.psiMemberAncestor(elt.getParent) match {
    case None => sys.error(elt + "memberAncestor not found")
    case Some(m : PsiMethod) =>
      anonymous(QualifiedName(m), BlockHolder(m.getBody))
    case Some(f : PsiField) =>
      anonymous(QualifiedName(f), ExprHolder(f.getInitializer))
  }

  def apply
  ( lvar: PsiLocalVariable)
  ( implicit builder: IntellijGraphBuilder ) : NodeId =
    psiMemberDefAncestorNode(lvar)

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
    case pct : PsiClassType => GetNode(pct.resolve())
    case ppt : PsiPrimitiveType =>
      val name = ppt.getCanonicalText(false)
      apply(s"$primitivePackage.$name", name, PrimitiveDeclHolder(ppt))
    case pat : PsiArrayType =>
      val name = Name(t)
      apply(s"$primitivePackage.$name", name, ArrayTypeWrapper)
    case _ =>
      sys.error(s"IntellijGraphBuilder.getTypeNode(t : ${t.getClass}) unhandled case")
  }
}
