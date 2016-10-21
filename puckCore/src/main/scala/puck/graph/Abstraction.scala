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

import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._
import ShowDG._

sealed abstract class AbstractionPolicy

case object DelegationAbstraction extends AbstractionPolicy

case object SupertypeAbstraction extends AbstractionPolicy


object Abstraction {

  def typeMemberOverridenAbstractions
  (g : DependencyGraph,
   tmImplId : NodeId) : Set[NodeId] = {

    val impl = g getConcreteNode tmImplId
    val implType = g typ tmImplId
    //val typedImpl = (, )

    def searchTroughHierarchy(t : NodeId,
                              acc : Set[NodeId]) : Set[NodeId] = {

      val candidates = g.content(t).toList

      val sabs = candidates find (cid => (g getConcreteNode cid).name == impl.name &&
        (g.styp(cid) exists (_.canOverride(g, implType)) ))

      val acc2 = sabs map (acc + _) getOrElse acc

      val superTypes = g.directSuperTypes(t).toList map Type.mainId

      superTypes.foldLeft(acc2){
        (acc3, superType) => searchTroughHierarchy(superType, acc3)
      }

    }

    searchTroughHierarchy(g container_! tmImplId, Set.empty) - tmImplId
  }

  def typeMembersFirstOverriddenAbstraction
  (g : DependencyGraph,
   typeAbsId : NodeId,
   tmImplsId : List[NodeId]
  ) : LoggedTry[Map[NodeId, NodeId]] = {
    /*precondition
      hostType(tmImplId) <: typeAbs
      \forall n \in tmImplsId => n is typed

        /!\ case de A :< B et A :< C avec méthode surchargé présente dans B ET C non géré
     */

    val typedImpls = tmImplsId map (n => (g getConcreteNode n, g typ n))
    //val log = s"tmAbstraction : searching an abstraction of ${(g, tmImpl).shows} in ${(g, typeAbsId).shows}\n"

    def findAbsInOneType(superType : NodeId,
                         impls : List[TypedNode],
                         map0 : Map[NodeId, NodeId]) : (List[TypedNode], Map[NodeId, NodeId]) = {
      val candidates = g.content(superType).toList
      val typedCandidates = candidates flatMap (c => g.styp(c) map ((g getConcreteNode c, _)))

      val (remainingImps, _, map) = impls.foldLeft((List[(ConcreteNode, Type)](), typedCandidates, map0)){
        case ((remainingSubs, remainingCandidates, map1), typedMeth @ (subMeth, subMethSig)) =>
          Type.findOverridingIn(g, subMeth.name, subMethSig, remainingCandidates) match {
            case None =>
              (typedMeth :: remainingSubs, remainingCandidates, map1)
            case Some(((absM,_), remainingCandidates1)) =>
              (remainingSubs, remainingCandidates1, map1 + (subMeth.id -> absM.id) )
          }
      }

      (remainingImps, map)
    }

    def searchTroughHierarchy(t : NodeId,
                              impls : List[TypedNode],
                              acc : Map[NodeId, NodeId]) : (List[TypedNode], Map[NodeId, NodeId]) = {
      val (remainings, acc2)= findAbsInOneType(t, impls, acc)
      if(remainings.isEmpty) (Nil, acc2)
      else {
        val superTypes = g.directSuperTypes(t).toList map Type.mainId
        superTypes.foldLeft((remainings, acc2)){
          case ((remainings2, acc3), superType) =>
            searchTroughHierarchy(superType, remainings2, acc3)
        }
      }
    }

    val (remainings, map) = searchTroughHierarchy(typeAbsId, typedImpls, Map())

    if(remainings.isEmpty) LoggedSuccess(map)
    else {
      val strs = remainings map {case (n, t) => (g,n).shows + " : " + (g, t).shows}
      LoggedError( s"abtraction not found for :${strs.mkString("\n", "\n", "\n")}" )
    }
  }

}

sealed abstract class Abstraction {
  def policy: AbstractionPolicy
  def nodes : List[NodeId]
  def kind(g : DependencyGraph) : NodeKind
  def kindType(g : DependencyGraph) : KindType
  def containerIn(g : DependencyGraph) :  Option[NodeId]
}

case class AccessAbstraction
( nodeId : NodeId,
  policy: AbstractionPolicy
) extends Abstraction {
  def nodes : List[NodeId] = List(nodeId)
  def kindType(g : DependencyGraph) : KindType = g.kindType(nodeId)
  def kind(g : DependencyGraph) : NodeKind = g.getNode(nodeId).kind
  def containerIn(g : DependencyGraph) : Option[NodeId] = g.container(nodeId)
}

case class ReadWriteAbstraction
( readAbsNode : Option[NodeId],
  writeAbsNode : Option[NodeId]
) extends Abstraction {
  def policy = DelegationAbstraction
  def nodes : List[NodeId] =
    List(readAbsNode, writeAbsNode).filter(_.nonEmpty).sequence.getOrElse(List())


  def bothSameValueOrElse[T](f: NodeId => T, default: => T) : T =
    (readAbsNode, writeAbsNode) match {
      case (None,None) => default
      case (Some(nodeId), None) =>  f(nodeId)
      case (None, Some(nodeId)) =>  f(nodeId)
      case (Some(nodeId0), Some(nodeId)) =>
        val v = f(nodeId)
        assert(f(nodeId0) == v)
        v
    }


  def kindType(g : DependencyGraph) : KindType =
    bothSameValueOrElse(g.kindType, UnknownKindType)

  def kind(g : DependencyGraph) : NodeKind =
    bothSameValueOrElse(g.getNode(_).kind, error("should have same kind"))


  def containerIn(g : DependencyGraph) =
    bothSameValueOrElse(g.container, None)

}
