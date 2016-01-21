package puck.graph


object EdgeMap {

  type AccessKindMap = Map[(NodeId, NodeId), UsesAccessKind]
  val AccessKindMap = Map

  type EdgeMapT = SetValueMap.T[NodeId, NodeId]
  val EdgeMapT = SetValueMap

  type ParamMapT = ListValueMap.T[NodeId, NodeId]
  val ParamMapT = ListValueMap


  type Node2NodeMap = Map[NodeId, NodeId]
  val Node2NodeMap = Map

  type UseDependencyMap = SetValueMap.T[NodeIdP, NodeIdP]
  val UseDependencyMap = SetValueMap


  def apply() =
    new EdgeMap(EdgeMapT(), EdgeMapT(),
                AccessKindMap(),
                EdgeMapT(), Node2NodeMap(),
                EdgeMapT(), EdgeMapT(),
                UseDependencyMap(),
                UseDependencyMap(),
                ParamMapT(),
                Map(), EdgeMapT())
}
import EdgeMap._
import puck.PuckError

case class EdgeMap
( userMap : EdgeMapT,
  usedMap  : EdgeMapT, //formely usesMap
  accessKindMap: AccessKindMap,
  //contains
  contents  : EdgeMapT,
  containers : Node2NodeMap,
  //isa
  superTypes : EdgeMapT,
  subTypes : EdgeMapT,
  //BR
  typeMemberUses2typeUsesMap : UseDependencyMap,
  typeUses2typeMemberUsesMap : UseDependencyMap,
  //special cases of contains :
  parameters : ParamMapT,
  //definition : Node2NodeMap,
  //special case of use
  types : Map[NodeId, Type],
  typedBy : EdgeMapT){

  def containsList : List[NodeIdP] =
    containers.toList map {case (cted, cter) => (cter, cted)}

  def allUsesList : List[NodeIdP] = usedMap.flatList ++ typeUsesList

  def typeUsesList : List[NodeIdP] = {
    for{
      nt <- types.toList
      (n , t) = nt
      i <- t.ids
    } yield (n,i)
  }

  def usedBy(userId : NodeId) : Set[NodeId] =
    /*(types get userId map (_.ids.toSet) getOrElse Set()) ++*/ (usedMap getFlat userId)

  def usersOf(usedId: NodeId) : Set[NodeId] =
    (userMap getFlat usedId) /*++ (typedBy getFlat usedId)*/

  def add(edge : DGEdge) : EdgeMap =
    edge match {
      case Uses(user, used, accK) =>

        copy(userMap = userMap + (used, user),
          usedMap = usedMap + (user, used),
          accessKindMap = newAccessKindMapOnAdd(user, used, accK))

      case Isa(subType, superType) =>
        copy(subTypes = subTypes + (superType, subType),
          superTypes = superTypes + (subType, superType))

      case Contains(container, content) =>
        copy(contents = contents + (container, content),
          containers = containers + (content -> container))

      case ContainsParam(decl, param) =>
        copy(parameters = parameters + (decl, param),
          containers = containers + (param -> decl) )
          /*declaration = declaration + (param -> decl) )*/
//      case ContainsDef(decl, _def) =>
//        copy(definition = definition + (decl -> _def),
//          containers = containers + (_def -> decl) )
          /*declaration = declaration + (_def -> decl) )*/
      case AbstractEdgeKind(_, _) => this
      
    }

  private def newAccessKindMapOnAdd
  ( user : NodeId,
    used : NodeId,
    accK : Option[UsesAccessKind]) : AccessKindMap =
    (accK, accessKindMap get ((user, used))) match {
      case (None, _) => accessKindMap - ((user, used))
      case (Some(ak1), None) => accessKindMap + ((user, used) -> ak1)
      case (Some(ak1), Some(ak2)) => accessKindMap + ((user, used) -> (ak1 && ak2))
    }



  def typedBy(nid : NodeId) = types.foldLeft(List[NodeId]()){
    case (l, (typed, NamedType(`nid`))) => typed :: l
    case (l, _) => l
  }

  def remove(edge : DGEdge) : EdgeMap =
    edge.kind match {
      case Uses =>
        copy(userMap = userMap - (edge.used, edge.user),
          usedMap = usedMap - (edge.user, edge.used),
          accessKindMap = accessKindMap - ((edge.user, edge.used)))
      case Isa =>
        copy(subTypes = subTypes - (edge.superType, edge.subType),
          superTypes = superTypes - (edge.subType, edge.superType))
      case Contains =>
        copy(contents = contents - (edge.container, edge.content),
          containers = containers - edge.content)

      case ContainsParam =>
        copy(parameters = parameters - (edge.container, edge.content),
          containers = containers - edge.content)
          /*declaration = declaration - edge.content )*/

//      case ContainsDef =>
//        copy(definition = definition - (edge.container, edge.content),
//          containers = containers - edge.content)
          /*declaration = declaration - edge.content )*/
      case AbstractEdgeKind => this
    }

  def contains(containerId : NodeId, contentId : NodeId) : Boolean =
    containers get contentId match {
      case None => false
      case Some(id) => id == containerId
    }

  def isa(subId : NodeId, superId: NodeId): Boolean = superTypes.bind(subId, superId)

  def isa_*(subId : NodeId, superId: NodeId): Boolean =
    isa(subId, superId) || {
      superTypes.getFlat(subId) exists (isa_*(_, superId))
    }


  def getUses(userId: NodeId, usedId: NodeId) : Option[Uses] = {
    if(uses(userId, usedId))
      Some(Uses(userId, usedId, accessKindMap get ((userId, usedId))))
    else
      None
  }

  def uses(userId: NodeId, usedId: NodeId) : Boolean =
    userMap.bind(usedId, userId) || {
      types get userId match {
        case None => false
        case Some(t) => t uses usedId
      }
    }

  def exists(e : DGEdge) : Boolean = e.kind  match {
    case Contains => contains(e.source, e.target)
    case ContainsParam => parameters get e.source match {
      case Some(ps) => ps contains e.target
      case None => false
    }
//    case ContainsDef => definition get e.source match {
//      case Some(d) => d == e.target
//      case None => false
//    }
    case Isa => isa(e.source, e.target)
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

  def setType(id : NodeId, st : Option[Type]) : EdgeMap =
    st match {
      case None =>
        types get id match {
          case None => this
          case Some(oldType) =>
            val newTypedBy = oldType.ids.foldLeft(typedBy){
            (tbm, tId) => tbm - (tId, id)
          }
          copy(types = types - id,
            typedBy = newTypedBy)
        }
      case Some(t) =>
        val newTypedBy = t.ids.foldLeft(typedBy){
          (tbm, tId) => tbm + (tId, id)
        }
        copy(types = types + (id -> t),
        typedBy = newTypedBy)
    }


  def typeUsesOf(typeMemberUse : Uses) : Set[Uses] =
    typeUsesOf(typeMemberUse.user, typeMemberUse.used)


  def typeMemberUsesOf(typeUse : Uses) : Set[Uses] =
    typeMemberUsesOf(typeUse.user, typeUse.used)

  def typeUsesOf(tmUser : NodeId, tmUsed : NodeId) : Set[Uses] =
    typeMemberUses2typeUsesMap getFlat ((tmUser, tmUsed)) map {
      case (s,t) =>
        try {
          getUses(s, t).get
        }catch {
          case _ : Throwable =>
            throw new PuckError(s"Uses($s, $t) does not exist !")
        }
    }


  def typeMemberUsesOf(typeUser : NodeId, typeUsed : NodeId) : Set[Uses] =
    typeUses2typeMemberUsesMap getFlat ((typeUser, typeUsed)) map {
      case (s, t) => getUses(s,t).get
    }

  
}