package puck.javaAG.immutable.nodeKind

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{HasType, NamedType}
import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction}
import JavaNodeKind._

/**
 * Created by lorilan on 31/07/14.
 */
case class Field private[javaAG](node : NodeId[JavaNodeKind],
                                 typ : NamedType[JavaNodeKind],
                                 decl : Option[AST.FieldDeclaration] )
  extends JavaNodeKind with HasType[JavaNodeKind, NamedType[JavaNodeKind]]{

  override val toString = "Field"

  def create(node : NodeId[JavaNodeKind]) = Field(node, typ, None)

 /* override def redirectUses(oldUsee : AGNode[JavaNodeKind],
                            newUsee : AGNode[JavaNodeKind]){
    if(typ.node == oldUsee)
      typ = new JavaNamedType(newUsee)
  }*/

  def canContain(k : JavaNodeKind) = false
  //TODO check abstraction : FieldRead != FieldWrite
  // fieldread abstraction type = () -> t
  // fielwrite abstraction type = t -> () (think of t -> t case of jrrt ... )
  def abstractKinds(p : AbstractionPolicy) = Seq(methodPrototype)

  override def abstractionPolicies = Seq(DelegationAbstraction())
}
