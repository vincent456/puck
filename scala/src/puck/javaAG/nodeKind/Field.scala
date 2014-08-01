package puck.javaAG.nodeKind

import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction}
import puck.graph.{AGNode, HasType}
import puck.javaAG.JavaType

/**
 * Created by lorilan on 31/07/14.
 */
case class Field private[javaAG]() extends JavaNodeKind with HasType[JavaType]{

  override val toString = "Field"

  def create() = JavaNodeKind.field(`type`)

  override def redirectUses(oldUsee : AGNode[JavaNodeKind],
                            newUsee : AGNode[JavaNodeKind]){
    if(`type`.node == oldUsee)
      `type` = new JavaType(newUsee)
  }

  var decl : AST.FieldDeclaration = _

  def canContain(k : JavaNodeKind) = false
  //TODO check abstraction : FieldRead != FieldWrite
  // fieldread abstraction type = () -> t
  // fielwrite abstraction type = t -> () (think of t -> t case of jrrt ... )
  def abstractKinds(p : AbstractionPolicy) = List(Method())

  override def abstractionPolicies = List(DelegationAbstraction())
}
