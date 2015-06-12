package puck.graph

import puck.graph.constraints.{DelegationAbstraction, AbstractionPolicy}

import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

sealed abstract class Abstraction {
  def policy: AbstractionPolicy
  def toList : List[NodeId]
}

case class AccessAbstraction
( node : NodeId,
  policy: AbstractionPolicy
  ) extends Abstraction {
  def toList : List[NodeId] = List(node)
}

case class ReadWriteAbstraction
( readAbsNode : Option[NodeId],
  writeAbsNode : Option[NodeId]
  ) extends Abstraction {
  def policy = DelegationAbstraction
  def toList : List[NodeId] =
    List(readAbsNode, writeAbsNode).filter(_.nonEmpty).sequence.getOrElse(List())
}
