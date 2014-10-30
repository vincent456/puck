package puck.javaAG.immutable.nodeKind

import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction}
import puck.graph.immutable.AccessGraph
import puck.graph.immutable.AccessGraph.NodeId

/**
 * Created by lorilan on 31/07/14.
 */
case class Package(node : NodeId[JavaNodeKind]) extends JavaNodeKind {
  override val toString = "Package"
  //val decl : AST.PackageDecl = _

  def create(node : NodeId[JavaNodeKind]) = Package(node)
  override def createDecl(prog : AST.Program,
                          graph : AccessGraph[JavaNodeKind]) : AccessGraph[JavaNodeKind] = graph


  def canContain(k : JavaNodeKind) : Boolean = {
    k match {
      case Package(_)
           | Class(_,_)
           | Interface(_,_) => true
      case _ => false
    }
  }

  override def abstractionPolicies = List(DelegationAbstraction())
  def abstractKinds(p : AbstractionPolicy) = List(JavaNodeKind.packageKind)

}
