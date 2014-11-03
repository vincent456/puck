package puck.javaAG.immutable.nodeKind

import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction}

/**
 * Created by lorilan on 31/07/14.
 */
case object Field extends JavaNodeKind {

  override val toString = "Field"

  def canContain(k : JavaNodeKind) = false
  //TODO check abstraction : FieldRead != FieldWrite
  // fieldread abstraction type = () -> t
  // fielwrite abstraction type = t -> () (think of t -> t case of jrrt ... )
  def abstractKinds(p : AbstractionPolicy) = Seq(Method)

  override def abstractionPolicies = Seq(DelegationAbstraction)
}
