package puck.graph

import puck.graph.constraints.{DelegationAbstraction, AbstractionPolicy}

import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

sealed abstract class Abstraction {
  def policy: AbstractionPolicy
  def nodes : List[NodeId]
  def kindType(g : DependencyGraph) : KindType
  def containerIn(g : DependencyGraph) :  Option[NodeId]
}

case class AccessAbstraction
( nodeId : NodeId,
  policy: AbstractionPolicy
  ) extends Abstraction {
  def nodes : List[NodeId] = List(nodeId)
  def kindType(g : DependencyGraph) : KindType = g.kindType(nodeId)
  def containerIn(g : DependencyGraph) : Option[NodeId] = g.container(nodeId)
}

case class ReadWriteAbstraction
( readAbsNode : Option[NodeId],
  writeAbsNode : Option[NodeId]
  ) extends Abstraction {
  def policy = DelegationAbstraction
  def nodes : List[NodeId] =
    List(readAbsNode, writeAbsNode).filter(_.nonEmpty).sequence.getOrElse(List())


  def bothSameValueOrElse[T](f: NodeId => T, default: T) : T =
    (readAbsNode, writeAbsNode) match {
      case (None,None) => default
      case (Some(nodeId), None) =>  f(nodeId)
      case (None, Some(nodeId)) =>  f(nodeId)
      case (Some(nodeId0),Some(nodeId)) =>
        val v = f(nodeId)
        assert(f(nodeId0) == v)
        v
    }


  def kindType(g : DependencyGraph) : KindType =
    bothSameValueOrElse(g.kindType, UnknownKindType)

  def containerIn(g : DependencyGraph) =
    bothSameValueOrElse(g.container, None)

}
