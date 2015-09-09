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
                EdgeMapT(), EdgeMapT(),
                EdgeMapT(), Node2NodeMap(),
                EdgeMapT(), EdgeMapT(),
                UseDependencyMap(),
                UseDependencyMap(),
                ParamMapT(), Node2NodeMap(), Map()/*, Node2NodeMap()*/)
}
import EdgeMap._
import puck.PuckError

case class EdgeMap
( userMap : EdgeMapT,
  usedMap  : EdgeMapT, //formely usesMap
  accessKindMap: AccessKindMap,
  parameterizedUsers : EdgeMapT,
  parameterizedUsed : EdgeMapT,
  contents  : EdgeMapT,
  containers : Node2NodeMap,
  superTypes : EdgeMapT,
  subTypes : EdgeMapT,
  typeMemberUses2typeUsesMap : UseDependencyMap,
  typeUses2typeMemberUsesMap : UseDependencyMap,
  //special cases of contains :
  parameters : ParamMapT,
  definition : Node2NodeMap/*,
  declaration : Node2NodeMap*/,
  types : Map[NodeId, Type]){

  override def toString : String = {
    val builder = new StringBuilder(150)

    builder.append("used -> user\n\t")
    builder.append(userMap.toString)
    builder.append("\nuser -> used\n\t")
    builder.append(usedMap.toString)

    builder.append("\npar used -> par user\n\t")
    builder.append(parameterizedUsers.toString)
    builder.append("\npar user -> par used\n\t")
    builder.append(parameterizedUsed.toString)

    builder.append("\ncontainer -> content\n\t")
    builder.append(contents.toString)
    builder.append("\ncontent -> container\n\t")
    builder.append(containers.toString())

    builder.append("\nsub -> super\n\t")
    builder.append(superTypes.toString)
    builder.append("\nsuper -> sub\n\t")
    builder.append(subTypes.toString)


    builder.append("\ntmUse -> tUse\n\t")
    builder.append(typeMemberUses2typeUsesMap.toString)
    builder.append("\ntUse -> tmUse\n\t")
    builder.append(typeUses2typeMemberUsesMap.toString)


    builder.toString()
  }




  def add(edge : DGEdge) : EdgeMap =
    edge match {
      case Uses(user, used, accK) =>

        copy(userMap = userMap + (used, user),
          usedMap = usedMap + (user, used),
          accessKindMap = newAccessKindMapOnAdd(user, used, accK))

      case ParameterizedUses(user, used, accK) =>
        copy(parameterizedUsers = parameterizedUsers + (used, user),
          parameterizedUsed = parameterizedUsed + (user, used),
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
      case ContainsDef(decl, _def) =>
        copy(definition = definition + (decl -> _def),
          containers = containers + (_def -> decl) )
          /*declaration = declaration + (_def -> decl) )*/
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



  def add(kind : DGEdge.EKind, source : NodeId, target : NodeId) : EdgeMap =
    add(kind(source, target))


  def remove(edge : DGEdge) : EdgeMap =
    edge.kind match {
      case Uses =>
        copy(userMap = userMap - (edge.used, edge.user),
          usedMap = usedMap - (edge.user, edge.used),
          accessKindMap = accessKindMap - ((edge.user, edge.used)))
      case ParameterizedUses =>
        copy(parameterizedUsers = parameterizedUsers - (edge.used, edge.user),
          parameterizedUsed = parameterizedUsed - (edge.user, edge.used))
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

      case ContainsDef =>
        copy(definition = definition - (edge.container, edge.content),
          containers = containers - edge.content)
          /*declaration = declaration - edge.content )*/
    }

  def remove(kind : DGEdge.EKind, source : NodeId, target : NodeId) : EdgeMap =
    remove(kind(source, target))

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


  def getUses(userId: NodeId, usedId: NodeId) : Option[DGUses] = {
    if(uses(userId, usedId))
      Some(Uses(userId, usedId, accessKindMap get ((userId, usedId))))
    else if(parameterizedUsers.bind(usedId, userId))
      Some(ParameterizedUses(userId, usedId, accessKindMap get ((userId, usedId))))
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

  def parUses(userId: NodeId, usedId: NodeId) : Boolean =
    parameterizedUsers.bind(usedId, userId)

  def exists(e : DGEdge) : Boolean = e.kind  match {
    case Contains => contains(e.source, e.target)
    case ContainsParam => parameters get e.source match {
      case Some(ps) => ps contains e.target
      case None => false
    }
    case ContainsDef => definition get e.source match {
      case Some(d) => d == e.target
      case None => false
    }
    case Isa => isa(e.source, e.target)
    case Uses => uses(e.source, e.target)
    case ParameterizedUses => parUses(e.source, e.target)
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
        if(types contains id) copy(types = types - id)
        else this
      case Some(t) => copy(types = types + (id -> t))
    }


  def typeUsesOf(typeMemberUse : DGUses) : Set[DGUses] =
    typeUsesOf(typeMemberUse.user, typeMemberUse.used)


  def typeMemberUsesOf(typeUse : DGUses) : Set[DGUses] =
    typeMemberUsesOf(typeUse.user, typeUse.used)

  def typeUsesOf(tmUser : NodeId, tmUsed : NodeId) : Set[DGUses] =
    typeMemberUses2typeUsesMap getFlat ((tmUser, tmUsed)) map {
      case (s,t) =>
        try {
          getUses(s, t).get
        }catch {
          case _ : Throwable =>
            throw new PuckError(s"Uses($s, $t) does not exist !")
        }
    }


  def typeMemberUsesOf(typeUser : NodeId, typeUsed : NodeId) : Set[DGUses] =
    typeUses2typeMemberUsesMap getFlat ((typeUser, typeUsed)) map {
      case (s, t) => getUses(s,t).get
    }

  
}