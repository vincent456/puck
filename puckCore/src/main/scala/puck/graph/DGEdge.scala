/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph
import puck.graph.DGEdge._


sealed abstract class EKind {
  def apply(pair : (NodeId, NodeId)) : DGEdge = this.apply(pair._1, pair._2)
  def apply(source : NodeId, target: NodeId): DGEdge
}

object DGEdge{

  implicit def toPair(e : DGEdge) : NodeIdP = (e.user, e.used)

  def concreteEdgesFrom(graph : DependencyGraph, virtual : NodeIdP) : List[DGEdge] = {
   val (source, target) = virtual
    val includedInVirtual : NodeIdP => Boolean = {
      case (user, used) => graph.contains_*(source, user) && graph.contains_*(target, used)
    }
    val concreteUses = graph.usesList filter includedInVirtual map Uses.apply

    (graph.isaList filter includedInVirtual map Isa.apply) ++ concreteUses

  }

  def pairOfKind(e: DGEdge, k : EKind) : Option[NodeIdP] =
    if(e.kind == k) Some((e.source, e.target))
    else None


}

case object AbstractEdgeKind extends EKind {
  def apply(source: NodeId, target: NodeId): DGEdge = new DGEdge(AbstractEdgeKind, source, target)
  override def toString = "Edge"
  def unapply(e: DGEdge) : Option[NodeIdP] = pairOfKind(e, AbstractEdgeKind)
}

case class DGEdge(kind : EKind, source : NodeId, target: NodeId) {
//  extends  Product2[NodeId, NodeId]{
//  def _1 = source
//  def _2 = target

  def copy(source : NodeId = source, target : NodeId = target) : DGEdge =
    new DGEdge(kind, source, target)

  type NIdT = NodeId
  /*
   * aliases for readibility
   */
  def user = source
  def used = target

  def container = source
  def content = target

  def subType = source
  def superType = target

  def selfUse : Boolean = source == target

  /*override def equals(obj:Any) : Boolean = obj match {
    case that : AGEdge[_] => this.kind == that.kind && this.source == that.source && this.target == that.target
    case _ => false
  }
  override def hashCode : Int = source.id + target.id*/

  override def toString : String = {
    kind + "( " + source + ", " + target + ")"
  }

  def existsIn(graph : DependencyGraph) = graph exists this

  def createIn(graph : DependencyGraph, register : Boolean = true) : DependencyGraph =
    graph.addEdge(this, register)

  def deleteIn(graph : DependencyGraph, register : Boolean = true) : DependencyGraph =
    graph.removeEdge(this, register)

  def changeTarget(graph : DependencyGraph, newTarget : NIdT) : DependencyGraph = graph.changeTarget(this, newTarget)
  def changeSource(graph : DependencyGraph, newSource : NIdT) : DependencyGraph = graph.changeSource(this, newSource)


}

case object Uses extends EKind {
  def apply(source: NodeId, target: NodeId) : DGEdge = new DGEdge(Uses, source, target)
  def unapply(e: DGEdge) : Option[(NodeId, NodeId)] = pairOfKind(e, Uses)
}

case object Isa extends EKind {
  def apply(source : NodeId, target: NodeId) : DGEdge = new DGEdge(Isa, source, target)
  def unapply(e: DGEdge) : Option[NodeIdP] = pairOfKind(e, Isa)
}


sealed abstract class ContainsKind extends EKind
object ContainsKind {
  def unapply(e: DGEdge) : Option[NodeIdP] =
    if (e.kind == Contains || e.kind == ContainsParam) Some((e.source, e.target))
    else None

}

case object Contains extends ContainsKind{
  def apply(source : NodeId, target: NodeId) : DGEdge = new DGEdge(Contains, source, target)
  def unapply(e: DGEdge) : Option[NodeIdP] = pairOfKind(e, Contains)
}

//Special cases of contains for handling in EdgeMap should not appear in the formalization :
case object ContainsParam extends ContainsKind {
  def apply(source : NodeId, target: NodeId) : DGEdge = new DGEdge(ContainsParam, source, target)
  def unapply(e: DGEdge) : Option[NodeIdP] = pairOfKind(e, ContainsParam)
}