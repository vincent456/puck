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

package puck.graph.transformations

import puck.graph._

object Operation {
  def involvedNodes(o : Operation) : Seq[NodeId] = o match {
    case VNode(n) => Seq(n.id)//n.id +: n.potentialMatches
    case CNode(cn) => Seq(cn.id)
    case Edge(e) => Seq(e.source, e.target)
    case Isa(t1, t2) => t1.ids ++ t2.ids
    case RedirectionOp(e, exty) => Seq(e.source, e.target, exty.node)
    case RenameOp(id, _, _) => Seq(id)
    case AType(id, t) => id +: t.ids
    case AbstractionOp(id, abs) => id :: abs.nodes
    case ChangeTypeBindingOp(((n1, n2), (n3, n4)), ext) =>
      Seq(ext.edge._1, ext.edge._2, n1, n2, n3, n4)
    case TypeBinding((n1, n2), (n3, n4)) =>
      Seq(n1, n2, n3, n4)
    case RoleChange(id, _, _) => Seq(id)
    case TypeConstraintOp(tc) => tc.typedNodes ++ tc.typeIds
    case AccessKind((tu, tmu),_) => Seq(tu.user, tu.used, tmu.user, tmu.used)
    case ChangeTypeOp(id,id2,id3) => Seq(id,id2,id3)

  }
}

sealed trait Operation {
  def execute(g: DependencyGraph , op : Direction) : DependencyGraph
  def productPrefix : String
}

sealed trait AddRmOperation extends Operation

case class CNode(n : ConcreteNode) extends AddRmOperation {

  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addConcreteNode(n)
    case Reverse => g.removeConcreteNode(n)
  }
}

case class VNode(n : VirtualNode) extends AddRmOperation {
  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addVirtualNode(n)
    case Reverse => g.removeVirtualNode(n)
  }
}

case class Edge(edge : DGEdge)
  extends AddRmOperation {
  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g addEdge edge
    case Reverse => g removeEdge edge
  }
}

case class Isa(subType : Type, superType : Type)
  extends AddRmOperation {
  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addIsa(subType, superType)
    case Reverse => g.removeIsa(superType, subType)
  }
}

sealed abstract class Extremity{
  val node : NodeId
  def create(n : NodeId) : Extremity
  def productPrefix : String
}

case class Source(node : NodeId) extends Extremity{
  def create(n : NodeId) : Extremity = Source(n)
}
case class Target(node : NodeId) extends Extremity  {
  def create(n : NodeId) : Extremity = Target(n)
}

case class RedirectionOp(edge : DGEdge, extremity : Extremity)
  extends Operation{

  val withMerge = false
  def execute(g: DependencyGraph , op : Direction) = (op, extremity) match {
    case (Regular, Target(newTarget)) => edge.changeTarget(g, newTarget)
    case (Reverse, Target(newTarget)) => edge.copy(target = newTarget).changeTarget(g, edge.target)
    case (Regular, Source(newSource)) => edge.changeSource(g, newSource)
    case (Reverse,Source(newSource)) => edge.copy(source = newSource).changeSource(g, edge.source)
  }
}

object RedirectionWithMerge {
  def unapply( t : RedirectionWithMerge) : Some[( DGEdge, Extremity)] =
    Some((t.edge, t.extremity))
}

class RedirectionWithMerge(edge : DGEdge, extremity : Extremity)
  extends RedirectionOp(edge, extremity){
  override val productPrefix = "RedirectionWithMerge"

  override def copy(edge : DGEdge = edge, extremity: Extremity = extremity) =
    new RedirectionWithMerge(edge, extremity)

  override val withMerge = true

  override def execute(g: DependencyGraph , op : Direction) = (op, extremity) match {
    case (Regular, _) => super.execute(g, op)
    case (Reverse, _) => edge.createIn(g)
  }

}

case class RenameOp
( nid : NodeId,
  oldName : String,
  newName : String)
extends Operation {
  override def execute(g: DependencyGraph , op: Direction): DependencyGraph  =
  op match {
    case Regular => g.setName(nid, newName)
    case Reverse => g.setName(nid, oldName)
  }
}


case class AType(typed : NodeId, t : Type) extends AddRmOperation {

  override def execute(g: DependencyGraph, op: Direction) = op match {
    case Regular => g.addType(typed, t)
    case Reverse => g.removeType(typed)
  }
}

case class ChangeTypeOp(typed : NodeId,
                        oldNamedTypeId : NodeId,
                        newNamedTypeId : NodeId) extends Operation {

  override def execute(g: DependencyGraph, op: Direction) = op match {
    case Regular => g.changeType((typed, oldNamedTypeId), newNamedTypeId)
    case Reverse => g.changeType((typed, newNamedTypeId), oldNamedTypeId)
  }
}

case class AbstractionOp
(impl: NodeId,
 abs : Abstraction)
 extends AddRmOperation {

  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addAbstraction(impl, abs)
    case Reverse => g.removeAbstraction(impl, abs)
  }
}



sealed abstract class BoundPart{
  val edge : NodeIdP
  def create(n : NodeIdP) : BoundPart
  def productPrefix : String
}
case class TypeUse(edge : NodeIdP) extends BoundPart{
  def create(e : NodeIdP) : BoundPart = TypeUse(e)
}
case class InstanceValueUse(edge : NodeIdP) extends BoundPart  {
  def create(e : NodeIdP) : BoundPart = InstanceValueUse(e)
}

case class ChangeTypeBindingOp(oldBinding : (NodeIdP,NodeIdP), extremity : BoundPart) extends Operation {

   def execute(g: DependencyGraph , op : Direction) = (op, extremity) match {
    case (Regular, TypeUse(tu)) =>
      g.changeTypeUseOfTypeMemberUse(oldBinding._1, tu, oldBinding._2)
    case (Reverse, TypeUse(tu)) =>
      g.changeTypeUseOfTypeMemberUse(tu, oldBinding._1, oldBinding._2)
    case (Regular, InstanceValueUse(tmu)) =>
      g.changeTypeMemberUseOfTypeUse(oldBinding._2, tmu, oldBinding._1)
    case (Reverse,InstanceValueUse(tmu)) =>
      g.changeTypeMemberUseOfTypeUse(tmu, oldBinding._2, oldBinding._1)
  }
}

case class TypeBinding
( typeUse : NodeIdP,
  typeMemberUse :  NodeIdP)
  extends AddRmOperation {
  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addBinding(typeUse, typeMemberUse)
    case Reverse => g.removeBinding(typeUse, typeMemberUse)
  }
}

case class AccessKind
( binding : (NodeIdP,NodeIdP),
  accK : UsesAccessKind)
extends AddRmOperation {
  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addAccessKind(binding, accK)
    case Reverse => g.rmAccessKind(binding)
  }
}

case class TypeConstraintOp
(constraint :  TypeConstraint)
  extends AddRmOperation {
  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addTypeConstraint(constraint)
    case Reverse => g.removeTypeConstraint(constraint)
  }
}

case class RoleChange
( id : NodeId,
  oldRole : Option[Role],
  newRole : Option[Role])
extends Operation {
  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.setRole(id, newRole)
    case Reverse => g.setRole(id, oldRole)
  }
}
