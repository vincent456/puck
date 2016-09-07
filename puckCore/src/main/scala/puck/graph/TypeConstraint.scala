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

/**
  * Created by Loïc Girault on 15/04/16.
  */

object TypeConstraint {

  def typeOf(g : DependencyGraph, tv : TypeConstraintVariable) : Type = tv match {
    case TypeOf(id) => g typ id
    case ConstrainedType(t) => t
    case ParTypeProjection(tv1, idx) =>
      val ParameterizedType(_, args) = typeOf(g, tv1)
      args(idx)
  }

  def comply(g : DependencyGraph, tuc : TypeConstraint) : Boolean = tuc match {
    case Eq(tv1, tv2) => typeOf(g, tv1) ==  typeOf(g, tv2)
    case Sub(sub, sup) => typeOf(g, sub).subtypeOf(g, typeOf(g, sup))
    case AndTypeConstraint(tcs) =>
      tcs.forall(comply(g,_))
  }
}


sealed abstract class TypeConstraintVariable {
  def typedNode : Option[NodeId]
}
case class TypeOf(nodeId: NodeId) extends TypeConstraintVariable {
  def typedNode : Option[NodeId] = Some(nodeId)
}
case class ConstrainedType(t : Type) extends TypeConstraintVariable {
  def typedNode : Option[NodeId] = None
}
case class ParTypeProjection(tv : TypeConstraintVariable, typeArgIndex : Int) extends TypeConstraintVariable{
  def typedNode : Option[NodeId] = tv.typedNode
}

object Typed {
  def unapply(arg: TypeConstraintVariable): Option[NodeId] =
    arg match {
      case TypeOf(nid) => Some(nid)
      case ConstrainedType(_) => None
      case ParTypeProjection(tv, _) => unapply(tv)
    }
}

sealed abstract class ConstraintOp
case object Sub extends ConstraintOp {
  def apply(sub : TypeConstraintVariable, sup : TypeConstraintVariable) : TypeConstraint =
    BinaryTypeConstraint(Sub, sub, sup)

  def unapply(tc: TypeConstraint): Option[(TypeConstraintVariable, TypeConstraintVariable)] = tc match {
    case BinaryTypeConstraint(Sub, l, r) => Some((l,r))
    case _ => None
  }
}
case object Eq extends ConstraintOp {
  def apply(l : TypeConstraintVariable, r : TypeConstraintVariable) : TypeConstraint =
    BinaryTypeConstraint(Eq, l, r)

  def unapply(tc: TypeConstraint): Option[(TypeConstraintVariable, TypeConstraintVariable)] = tc match {
    case BinaryTypeConstraint(Eq, l, r) => Some((l,r))
    case _ => None
  }
}

sealed abstract class TypeConstraint {
  def typedNodes : List[NodeId]
}
case class BinaryTypeConstraint(op : ConstraintOp, left : TypeConstraintVariable, right : TypeConstraintVariable) extends TypeConstraint {
  def typedNodes : List[NodeId] =
    (left.typedNode, right.typedNode) match {
      case (None, None) => List()
      case (Some(id), None) => List(id)
      case (None, Some(id)) => List(id)
      case (Some(id1), Some(id2)) =>
        if(id1 == id2) List(id1)
        else List(id1, id2)
    }
}

case class AndTypeConstraint(cts : List[TypeConstraint]) extends TypeConstraint {
  def typedNodes : List[NodeId] =
    cts.foldLeft(Set[NodeId]()) {
      case (s, ct) => s ++ ct.typedNodes
    }.toList
}

//case class ParamaterizedAnd(arg : Type, tcs : List[TypeConstraint]) extends TypeConstraint


//object TypeConstraint {
//  def extendedForConstraint(graph : DependencyGraph, decl : NodeId, rightExpr : NodeId ) : TypeConstraint = {
//
//  }
//}