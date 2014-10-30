package puck.javaAG.immutable.nodeKind

import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}
import puck.graph.AGError
import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{AccessGraph, HasType}
import puck.javaAG.immutable.MethodType

/**
 * Created by lorilan on 31/07/14.
 */

case class Constructor private[javaAG]
( node : NodeId[JavaNodeKind],
  typ : MethodType.T,
  decl : Option[AST.ConstructorDecl])
  extends JavaNodeKind with HasType[JavaNodeKind, MethodType.T]{

  override val toString = "Constructor"

  def create(node : NodeId[JavaNodeKind]) = Constructor(node, typ, None)


  def canContain(k : JavaNodeKind) = false

  override def abstractionPolicies = List(DelegationAbstraction())

  def abstractKinds(p : AbstractionPolicy) = p match {
    case DelegationAbstraction() => List( new ConstructorMethod(AccessGraph.dummyId, typ, None, None))
    case SupertypeAbstraction() => throw new AGError("Constructor cannot be abstracted by SuperType strategy")
  }

}
