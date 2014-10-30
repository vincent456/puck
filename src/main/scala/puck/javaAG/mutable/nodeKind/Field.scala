package puck.javaAG.mutable.nodeKind

import puck.graph.constraints.{DelegationAbstraction, AbstractionPolicy}
import puck.graph.mutable.{NamedType, AGNode, HasType}
import puck.javaAG.mutable.JavaNamedType

/**
 * Created by lorilan on 31/07/14.
 */
case class Field private[javaAG]() extends JavaNodeKind with HasType[JavaNodeKind, NamedType[JavaNodeKind]]{

  override val toString = "Field"

  def create() = JavaNodeKind.field(typ)

  override def redirectUses(oldUsee : AGNode[JavaNodeKind],
                            newUsee : AGNode[JavaNodeKind]){
    if(typ.node == oldUsee)
      typ = new JavaNamedType(newUsee)
  }

  var decl : AST.FieldDeclaration = _

  def canContain(k : JavaNodeKind) = false
  //TODO check abstraction : FieldRead != FieldWrite
  // fieldread abstraction type = () -> t
  // fielwrite abstraction type = t -> () (think of t -> t case of jrrt ... )
  def abstractKinds(p : AbstractionPolicy) = List(Method())

  override def abstractionPolicies = List(DelegationAbstraction())
}
