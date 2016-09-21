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


object EdgeMap {

  type AccessKindMap = Map[(NodeIdP, NodeIdP), UsesAccessKind]
  val AccessKindMap = Map

  type EdgeMapT = SetValueMap[NodeId, NodeId]
  val EdgeMapT = SetValueMap

  type ParamMapT = ListValueMap[NodeId, NodeId]
  val ParamMapT = ListValueMap

  type TypeMapT = SetValueMap[NodeId, Type]
  val TypeMapT = SetValueMap


  type Node2NodeMap = Map[NodeId, NodeId]
  val Node2NodeMap = Map

  type UseDependencyMap = SetValueMap[NodeIdP, NodeIdP]
  val UseDependencyMap = SetValueMap


  def apply() =
    new EdgeMap(EdgeMapT(), EdgeMapT(),
                AccessKindMap(),
                EdgeMapT(), Node2NodeMap(),
                EdgeMapT(), EdgeMapT(),
                UseDependencyMap(),
                UseDependencyMap(),
                SetValueMap(),
                ParamMapT(),
                Map(), EdgeMapT())
}
import EdgeMap._
import puck.NonExistentEdge

case class EdgeMap
(userMap : EdgeMapT,
 usedMap  : EdgeMapT,
 accessKindMap: AccessKindMap,
 //contains
 contents  : EdgeMapT,
 containers : Node2NodeMap,
 //isa
 superTypes : TypeMapT,
 subTypes : TypeMapT,
 //BR
 typeMemberUses2typeUsesMap : UseDependencyMap,
 typeUses2typeMemberUsesMap : UseDependencyMap,
 typeConstraints : SetValueMap[NodeId, TypeConstraint],
 // if (a, Sub(b)) then constraint( type(a) :> type(b) )
 // if ((a, t), Sup(a, s)) then constraint( t <: s )
 // if ((a, t), Eq(a, s)) then constraint( t =:= s )

 //special cases of contains :
 parameters : ParamMapT,
 //definition : Node2NodeMap,
 //special case of use
 types : Map[NodeId, Type],
 typedBy : EdgeMapT){

  def containsList : List[NodeIdP] =
    containers.toList map (_.swap)


  def allUsesList : List[NodeIdP] = usedMap.flatList ++ typeUsesList ++ hierarchyTypeUsesList
  def usesListExludingTypeUses : List[NodeIdP] = usedMap.flatList
  def typeUsesList : List[NodeIdP] =
    for {
      (n , t) <- types.toList
      i <- t.ids
    } yield (n,i)

  def hierarchyTypeUsesList : List[NodeIdP] =
    for {
      (n , ts) <- superTypes.toList
      t <- ts
      i <- t.ids
    } yield (n, i)


  def usedByExcludingTypeUse(userId : NodeId) : Set[NodeId] = usedMap getFlat userId

  def usersOfExcludingTypeUse(usedId: NodeId) : Set[NodeId] = userMap getFlat usedId

  def usedBy(userId : NodeId) : Set[NodeId] =
  (types get userId map (_.ids.toSet) getOrElse Set()) ++ (usedMap getFlat userId)

  def usersOf(usedId: NodeId) : Set[NodeId] =
    (userMap getFlat usedId) ++ typedBy(usedId).toSet

  def add(edge : DGEdge) : EdgeMap =
    edge match {
      case Uses(user, used) =>
        copy(userMap = userMap + (used, user),
          usedMap = usedMap + (user, used))

      case Contains(container, content) =>
        copy(contents = contents + (container, content),
          containers = containers + (content -> container))

      case ContainsParam(decl, param) =>
        copy(parameters = parameters + (decl, param),
          containers = containers + (param -> decl) )

      case AbstractEdgeKind(_, _) => this
      
    }

  private def newAccessKindMapOnAdd
  ( br : (NodeIdP, NodeIdP),
    accK : Option[UsesAccessKind]) : AccessKindMap =
    (accK, accessKindMap get br) match {
      case (Some(ak1), None) => accessKindMap + (br -> ak1)
      case (Some(ak1), Some(ak2)) => accessKindMap + (br -> (ak1 && ak2))
      case (None, _) => accessKindMap - br
    }

  def changeAccessKind(br : (NodeIdP, NodeIdP),
                    accK : Option[UsesAccessKind]) =
    copy(accessKindMap = newAccessKindMapOnAdd(br, accK))


  def typedBy(nid : NodeId) = types.foldLeft(List[NodeId]()){
    case (l, (typed, NamedType(`nid`))) => typed :: l
    case (l, _) => l
  }

  def remove(edge : DGEdge) : EdgeMap =
    edge.kind match {
      case Uses =>
        copy(userMap = userMap - (edge.used, edge.user),
          usedMap = usedMap - (edge.user, edge.used))

      case Contains =>
        copy(contents = contents - (edge.container, edge.content),
          containers = containers - edge.content)

      case ContainsParam =>
        copy(parameters = parameters - (edge.container, edge.content),
          containers = containers - edge.content)

      case AbstractEdgeKind => this
    }

  def contains(containerId : NodeId, contentId : NodeId) : Boolean =
    containers get contentId match {
      case None => false
      case Some(id) => id == containerId
    }

  def addIsa(subType : Type, superType : Type) = {
    val subTypeId = Type.mainId(subType)
    val superTypeId = Type.mainId(superType)
    copy(subTypes = subTypes + (superTypeId, subType),
      superTypes = superTypes + (subTypeId, superType))
  }

  def removeIsa(subType : Type, superType : Type) = {
    val subTypeId = Type.mainId(subType)
    val superTypeId = Type.mainId(superType)

    copy(subTypes = subTypes - (superTypeId, subType),
      superTypes = superTypes - (subTypeId, superType))
  }

  def isa(sub : Type, sup: Type): Boolean =
    superTypes getFlat (Type mainId sub) contains sup

  def isa_*(sub : Type, sup: Type): Boolean =
    sub == sup ||
      isa(sub, sup) || {
      superTypes getFlat (Type mainId sub) exists (isa_*(_, sup))
    }

  def isa(subId : NodeId, superId: NodeId): Boolean =
    superTypes getFlat subId map Type.mainId contains superId

  def isa_*(subId : NodeId, superId: NodeId): Boolean =
    subId == superId ||
    isa(subId, superId) || {
      superTypes getFlat subId map Type.mainId exists (isa_*(_, superId))
    }


  def getAccessKind(br: (NodeIdP, NodeIdP))  : Option[UsesAccessKind] =
    accessKindMap get br

  def uses(use : NodeIdP) : Boolean = uses(use.user, use.used)

  def uses(userId: NodeId, usedId: NodeId) : Boolean =
    userMap.bind(usedId, userId) || {
      types get userId match {
        case None => false
        case Some(t) => t uses usedId
      }
    } || (superTypes get userId match {
      case None => false
      case Some(supTypes) => supTypes exists ( t => t.ids contains usedId)
    })

  def exists(e : DGEdge) : Boolean = e.kind  match {
    case Contains => contains(e.source, e.target)
    case ContainsParam => parameters get e.source match {
      case Some(ps) => ps contains e.target
      case None => false
    }

    //case Isa => isa(e.source, e.target)
    case Uses => uses(e.source, e.target)
    case AbstractEdgeKind => false
  }




  def addUsesDependency(typeUse : NodeIdP,
                        typeMemberUse : NodeIdP) : EdgeMap =
    copy(typeMemberUses2typeUsesMap = typeMemberUses2typeUsesMap + (typeMemberUse, typeUse),
      typeUses2typeMemberUsesMap = typeUses2typeMemberUsesMap + (typeUse, typeMemberUse))


  def removeUsesDependency(typeUse : NodeIdP,
                           typeMemberUse : NodeIdP) : EdgeMap =
    copy(typeMemberUses2typeUsesMap = typeMemberUses2typeUsesMap - (typeMemberUse, typeUse),
      typeUses2typeMemberUsesMap = typeUses2typeMemberUsesMap - (typeUse, typeMemberUse))

  def addTypeConstraint(tc : TypeConstraint) : EdgeMap =
    copy(typeConstraints = typeConstraints ++ tc.typedNodes.map((_, tc)))

  def removeTypeConstraint(tc : TypeConstraint) : EdgeMap =
    copy(typeConstraints = typeConstraints -- tc.typedNodes.map((_, tc)))



  def redirectTypeUse(typed : NodeId, oldTypeUsed : NodeId, newTypeUsed : NodeId) : EdgeMap =
     types get typed map { t =>
       val newType = t.changeNamedType(oldTypeUsed, newTypeUsed)
       this.copy(types = types + (typed -> newType))
     } getOrElse this



  def setType(typed : NodeId, t : Type) : EdgeMap = this.copy (types = types + (typed -> t))
  def removeType(typed: NodeId) : EdgeMap = this.copy(types = types - typed)



  def typeMemberUsesOf(typeUse : NodeIdP) : Set[NodeIdP] =
    typeMemberUsesOf(typeUse.user, typeUse.used)

  def typeMemberUsesOf(typeUser : NodeId, typeUsed : NodeId) : Set[NodeIdP] =
    typeUses2typeMemberUsesMap getFlat ((typeUser, typeUsed)) map {
      case u @ (s,t) if uses(s,t) => u
      case (s,t) => throw NonExistentEdge(Uses(s,t))
    }


  def typeUsesOf(typeMemberUse : NodeIdP) : Set[NodeIdP] =
    typeUsesOf(typeMemberUse.user, typeMemberUse.used)

  def typeUsesOf(tmUser : NodeId, tmUsed : NodeId) : Set[NodeIdP] =
    typeMemberUses2typeUsesMap getFlat ((tmUser, tmUsed)) map {
      case u @ (s,t) if uses(s,t) => u
      case (s,t) => throw NonExistentEdge(Uses(s,t))
    }



  
}