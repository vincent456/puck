package puck.javaGraph.transformations

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.transformations.MergeMatcher
import puck.javaGraph.nodeKind.{Interface, AbstractMethod}

/**
 * Created by lorilan on 3/18/15.
 */

object InterfaceMergeMatcher {

  implicit def mergeMatcher(n : ConcreteNode): MergeMatcher = n.kind match {
    case Interface => new InterfaceMergeMatcher(n)
    case AbstractMethod => new AbstractMethodMergeMatcher(n)
    case _ => new MergeMatcher(){
      override val node = n
      override def canBeMergedInto(other : ConcreteNode, graph : DependencyGraph) = false
    }
  }

  def findMergingCandidateIn(g : DependencyGraph, method : ConcreteNode, interface : ConcreteNode) : Option[NodeId] =
    method.styp match {
      case None => throw new DGError("Method must have a type")
      case Some(t) =>
        val m = method.copy(styp = Some(t.redirectUses(g.container(method.id).get, interface)))
        g.content(interface.id).find { ncId =>
          m.canBeMergedInto(g.getConcreteNode(ncId), g)
        }
    }
}

import InterfaceMergeMatcher._


class InterfaceMergeMatcher(val node : ConcreteNode) extends MergeMatcher {



  override def canBeMergedInto(other : ConcreteNode, graph : DependencyGraph): Boolean =
    super.canBeMergedInto(other, graph) &&
      areMergingCandidates(node.id, other.id, graph)

  def areMergingCandidates(interface1 : NodeId, interface2: NodeId, g : DependencyGraph): Boolean = {

    def hasMatchingMethod(absmId : NodeId) = {
      val absm = g.getConcreteNode(absmId)
      absm.kind match {
        case AbstractMethod => findMergingCandidateIn(g, absm, g.getConcreteNode(interface2)).isDefined
        case _ =>
          g.logger.writeln("searching for merging candidate "+
            s"interface ${showDG[NodeId](g).shows(interface1)} contains ${showDG[NodeId](g).shows(absmId)}\n")
          true
      }
    }


    //the two interface are structurally compatible to merge
    g.content(interface2).size >= g.content(interface1).size &&
      (g.content(interface1) forall hasMatchingMethod) &&
      (g.content(interface2).size == g.content(interface1).size ||
        { g.directSubTypes(interface1).forall(g.isSuperTypeOf(interface2,_))
          //TODO structual type check
          /*val missingMethodsInThis =
            otherItc.content.filterNot{hasMatchingMethodIn(this)}*/
        }) ||
      //the two interfaces introduced an uneeded level of indirection
      g.isa(interface1, interface2) &&
        g.directSubTypes(interface1).forall(g.isSuperTypeOf(interface2,_))

  }
}

class FieldMergeMatcher(val node : ConcreteNode)extends MergeMatcher {
  override def canBeMergedInto(other : ConcreteNode, graph : DependencyGraph) : Boolean = {
    super.canBeMergedInto(other, graph) && other.styp == node.styp
  }
}

class AbstractMethodMergeMatcher(val node : ConcreteNode) extends MergeMatcher {
  override def canBeMergedInto(other : ConcreteNode, graph : DependencyGraph) : Boolean =
    super.canBeMergedInto(other, graph) &&
      other.name == node.name && other.styp == node.styp
}