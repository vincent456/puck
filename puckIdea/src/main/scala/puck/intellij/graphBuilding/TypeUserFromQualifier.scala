package puck.intellij.graphBuilding

import com.intellij.psi._
import puck.graph._

/**
 * Created by lorilan on 28/10/15.
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
