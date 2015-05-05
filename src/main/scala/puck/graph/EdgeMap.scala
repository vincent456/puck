package puck.graph

import puck.graph.DGEdge.{ContainsK, IsaK, ParameterizedUsesK, UsesK}



object EdgeMap {

  type EdgeMapT = SetValueMap[NodeId, NodeId]
  val EdgeMapT = SetValueMap

  type Node2NodeMap = Map[NodeId, NodeId]
  val Node2NodeMap = Map

  type UseDependencyMap = SetValueMap[DGUses, DGUses]
  val UseDependencyMap = SetValueMap


  def apply() =
    new EdgeMap(EdgeMapT(), EdgeMapT(),
                EdgeMapT(), EdgeMapT(),
                EdgeMapT(), Node2NodeMap(),
                EdgeMapT(), EdgeMapT(),
                UseDependencyMap(),
                UseDependencyMap())
}
import EdgeMap._
case class EdgeMap
( users : EdgeMapT,
  used  : EdgeMapT, //formely usesMap
  parameterizedUsers : EdgeMapT,
  parameterizedUsed : EdgeMapT,
  contents  : EdgeMapT,
  containers : Node2NodeMap,
  superTypes : EdgeMapT,
  subTypes : EdgeMapT,
  typeMemberUses2typeUsesMap : UseDependencyMap,
  typeUses2typeMemberUsesMap : UseDependencyMap ){

  override def toString : String = {
    val builder = new StringBuilder(150)

    builder.append("used -> user\n\t")
    builder.append(users.toString)
    builder.append("\nuser -> used\n\t")
    builder.append(used.toString)

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
    edge.kind match {
      case UsesK =>
        copy(users = users + (edge.used, edge.user),
          used = used + (edge.user, edge.used))
      case ParameterizedUsesK =>
        copy(parameterizedUsers = parameterizedUsers + (edge.used, edge.user),
          parameterizedUsed = parameterizedUsed + (edge.user, edge.used))
      case IsaK =>
        copy(subTypes = subTypes + (edge.superType, edge.subType),
          superTypes = superTypes + (edge.subType, edge.superType))
      case ContainsK =>
        copy(contents = contents + (edge.container, edge.content),
          containers = containers + (edge.content -> edge.container))
    }

  def add(kind : DGEdge.EKind, source : NodeId, target : NodeId) : EdgeMap =
    add(kind(source, target))


  def remove(edge : DGEdge) : EdgeMap =
    edge.kind match {
      case UsesK =>
        copy(users = users - (edge.used, edge.user),
          used = used - (edge.user, edge.used))
      case ParameterizedUsesK =>
        copy(parameterizedUsers = parameterizedUsers - (edge.used, edge.user),
          parameterizedUsed = parameterizedUsed - (edge.user, edge.used))
      case IsaK =>
        copy(subTypes = subTypes - (edge.superType, edge.subType),
          superTypes = superTypes - (edge.subType, edge.superType))
      case ContainsK =>
        copy(contents = contents - (edge.container, edge.content),
          containers = containers - edge.content)
    }

  def remove(kind : DGEdge.EKind, source : NodeId, target : NodeId) : EdgeMap =
    remove(kind(source, target))

  def contains(containerId : NodeId, contentId : NodeId) : Boolean =
    containers get contentId match {
      case None => false
      case Some(id) => id == containerId
    }

  def isa(subId : NodeId, superId: NodeId): Boolean = superTypes.bind(subId, superId)

  def uses(userId: NodeId, usedId: NodeId) : Boolean = users.bind(usedId, userId)

  def exists(e : DGEdge) : Boolean = e.kind  match {
    case ContainsK => contains(e.source, e.target)
    case IsaK => isa(e.source, e.target)
    case UsesK => uses(e.source, e.target)
    case ParameterizedUsesK => parameterizedUsers.bind(e.source, e.target)
  }


  def addUsesDependency(typeUse : DGUses,
                        typeMemberUse : DGUses) : EdgeMap =
    copy(typeMemberUses2typeUsesMap = typeMemberUses2typeUsesMap + (typeMemberUse, typeUse),
      typeUses2typeMemberUsesMap = typeUses2typeMemberUsesMap + (typeUse, typeMemberUse))

  def removeUsesDependency(typeUse : DGUses,
                           typeMemberUse : DGUses) : EdgeMap =
    copy(typeMemberUses2typeUsesMap = typeMemberUses2typeUsesMap - (typeMemberUse, typeUse),
      typeUses2typeMemberUsesMap = typeUses2typeMemberUsesMap - (typeUse, typeMemberUse))


  def typeUsesOf(typeMemberUse : DGUses) : Set[DGUses] =
    typeMemberUses2typeUsesMap getFlat typeMemberUse

  def typeMemberUsesOf(typeUse : DGUses) : Set[DGUses] =
    typeUses2typeMemberUsesMap  getFlat typeUse

  def typeUsesOf(tmUser : NodeId, tmUsed : NodeId) : Set[DGUses] =
    typeMemberUses2typeUsesMap getFlat Uses(tmUser, tmUsed) union
      (typeMemberUses2typeUsesMap getFlat ParameterizedUses(tmUser, tmUsed))

  def typeMemberUsesOf(typeUser : NodeId, typeUsed : NodeId) : Set[DGUses] =
    typeUses2typeMemberUsesMap getFlat Uses(typeUser, typeUsed) union
      (typeUses2typeMemberUsesMap getFlat ParameterizedUses(typeUser, typeUsed))
  
}