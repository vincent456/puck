package puck.javaAG.mutable.nodeKind

import puck.graph.mutable.AGNode
import puck.graph.mutable.constraints.{AbstractionPolicy, DelegationAbstraction}

/**
 * Created by lorilan on 31/07/14.
 */
case class Package() extends JavaNodeKind {
  override val toString = "Package"

  def create() = Package()
  override def createDecl(n : AGNode[JavaNodeKind]){}
  //var decl : AST.PackageDecl = _

  def canContain(k : JavaNodeKind) : Boolean = {
    k match {
      case Package()
           | Class()
           | Interface() => true
      case _ => false
    }
  }

  override def abstractionPolicies = List(DelegationAbstraction())
  def abstractKinds(p : AbstractionPolicy) = List(JavaNodeKind.`package`)

}
