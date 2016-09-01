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

import puck.NodeError
import puck.graph.transformations.rules.Intro

trait NodeKind {
  def canContain(k : NodeKind) : Boolean
  def canBe(k : NodeKind) : Boolean = false
  def abstractionPolicies : Seq[AbstractionPolicy] =
    Seq(SupertypeAbstraction, DelegationAbstraction)

  def isWritable : Boolean = false

  def abstractionNodeKinds(p : AbstractionPolicy) : Seq[NodeKind]

  def canBeAbstractedWith(p: AbstractionPolicy) =
    abstractionNodeKinds(p).nonEmpty

  def abstractionChoices : Seq[(NodeKind, AbstractionPolicy)] =
    for {
      p <- abstractionPolicies
      k <- abstractionNodeKinds(p)
    } yield (k, p)

  def kindType : KindType
}

object KindType {
  val isStatic : KindType => Boolean = {
    case NameSpace
         | TypeDecl
         | StableValue
         //| StableVariable
         | TypeConstructor => true
    case TypeVariableKT
         | InstanceValue
         //| InstanceVariable
         | InstanceTypeDecl => false
    case Parameter
         | ValueDef
         | UnknownKindType => ???
  }
  def isInstance(kt : KindType) : Boolean = !isStatic(kt)
}


sealed trait KindType
case object UnknownKindType extends KindType
case object NameSpace extends KindType

case object TypeDecl extends KindType //StableType
case object TypeVariableKT extends KindType
case object InstanceTypeDecl extends KindType

case object TypeConstructor extends KindType

//case object InstanceVariable extends KindType
//case object StableVariable extends KindType

case object InstanceValue extends KindType
case object StableValue extends KindType

case object Parameter extends KindType
case object ValueDef extends KindType

object Role{
  def isFactory(dg : DependencyGraph, cn : ConcreteNode) : Boolean =
    dg.getRole(cn.id) match {
      case Some(Factory(_)) => true
      case _ => false
    }

  def isInitializer(dg : DependencyGraph, cn : ConcreteNode) : Boolean =
    dg.getRole(cn.id) match {
      case Some(Initializer(_)) => true
      case _ => false
    }
}

sealed abstract class Role
case class Initializer(typeDecl : NodeId) extends Role
case class Factory(constructor: NodeId) extends Role
//case class Getter(field : NodeId) extends Role
//case class Setter(field : NodeId) extends Role


trait AGRoot extends NodeKind {
  def canContain(k: NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) =
    throw new DGError("Root node cannot be abstracted")

  def kindType = NameSpace
}

trait NodeKindKnowledge {

  def root : ConcreteNode

  def nodeKinds : List[NodeKind]

  def lightKind : NodeKind

  def canContain
  ( graph : DependencyGraph,
    n : DGNode,
    other : ConcreteNode) : Boolean =
    !graph.contains_*(other.id, n.id) && // no cycle !
      canContain(graph, n, other.kind)


  def canContain
  ( graph : DependencyGraph,
    n : DGNode,
    otherKind : NodeKind): Boolean =
    n.kind canContain otherKind

  def kindOfKindType(kindType: KindType) : Seq[NodeKind]

  def canBe
  ( graph : DependencyGraph,
    n : DGNode, other : ConcreteNode): Boolean = {
    !graph.isa_*(other.id, n.id) && // no cycle !
      (n.kind canBe other.kind)
  }

  def unitType(graph : DependencyGraph) : Type // or void "type"
  def writeType(graph : DependencyGraph, writtenValue : NodeId) : Type

  def defaultKindForNewReceiver : NodeKind

  def initializerKind : NodeKind

  def intro : Intro

  def getConstructorOfType(g: DependencyGraph, tid : NodeId) : Option[NodeId]

  def structuredType(graph : DependencyGraph, id : NodeId, params : List[NodeId]) : Option[Type] = {
    //in case of generic method, the type variables it owns precede the parameters
    val ps = params.dropWhile(graph.kindType(_) == TypeVariableKT)

    //assert node is a typed value
    if(ps.isEmpty) graph styp id
    else {
      Some(Arrow(Tuple(ps map (pid => graph styp pid getOrElse( throw new NodeError(pid, graph.fullName(pid) + " has no type !")))),
        graph styp id getOrElse( throw new NodeError(id, graph.fullName(id) + " has no type !"))))
    }
  }
  
  

}