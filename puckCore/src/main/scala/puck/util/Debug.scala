package puck.util

import puck.graph._
import ShowDG._

import scala.math.Ordering


object Debug {


  def printEdgeSet(g : DependencyGraph, logger : PuckLogger, s : Set[DGUses])=
    s.foreach(e => logger.writeln(s"\t\t*${(g, e).shows}"))

  def logUsersOf(g : DependencyGraph, logger : PuckLogger, n : NodeId) = {

    logger.writeln(s"users of ${(g, n).shows} :")

    def printUser = g.kindType(n) match {
      case InstanceValueDecl =>
        (userId : NodeId) =>
          logger.writeln("\tType Uses are:")
          printEdgeSet(g, logger, g.typeUsesOf(userId,n))
      case TypeDecl =>
        (userId : NodeId) =>
          logger.writeln("\tTypeMember Uses are:")
          printEdgeSet(g, logger, g.typeMemberUsesOf(userId,n))
      case kt =>
        logger.writeln(s"$kt (${g.getNode(n).kind}) unhandled")
        (userId : NodeId) => ()
    }

    g.usersOf(n).foreach {
      userId =>
        logger.writeln(s"\t- user ${(g, userId).shows}")
        printUser(userId)
    }

  }

  def logUsedBy(g : DependencyGraph, logger : PuckLogger, n : NodeId) = {
    logger.writeln(s"used by ${(g, n).shows} :")

    g.usedBy(n).foreach{
      usedId =>
        g.kindType(usedId) match {
          case InstanceValueDecl =>
            logger.writeln(s"\t- used type member ${(g, usedId).shows}")
            logger.writeln("\tType Uses are:")
            printEdgeSet(g, logger, g.typeUsesOf(n, usedId))
          case TypeDecl =>
            logger.writeln(s"\t- used type ${(g, usedId).shows}")
            logger.writeln("\tTypeMember Uses are:")
            printEdgeSet(g, logger, g.typeMemberUsesOf(n, usedId))
          case kt => logger.writeln(s"$kt (${g.getNode(usedId).kind}) unhandled")

        }

    }
  }

  def logUsesDependency(g : DependencyGraph, logger : PuckLogger, n : NodeId) = {
    logUsersOf(g, logger, n)
    logUsedBy(g, logger, n)

  }

  def mkMapStringSortedByKey[A,B](m : Map[A,B])(implicit ord: Ordering[A]) : String = {
    m.toList.sortBy(_._1).mkString("\t[",",\n\t ","]\n")

  }


  implicit val showNodeIndex : scalaz.Show[NodeIndex] = scalaz.Show.shows[NodeIndex] {
    case NodeIndex(_, cNodes, removedCnodes,
    vNodes, removedVnodes,
    cNodes2vNodes,
    roles) =>
      "Concrete Nodes : " +
        mkMapStringSortedByKey(cNodes) +
      "Removed Concrete Nodes : " +
        mkMapStringSortedByKey(removedCnodes) +
      "Virtual Nodes : " +
        mkMapStringSortedByKey(vNodes) +
      "Removed Virtual Nodes : " +
        mkMapStringSortedByKey(removedVnodes) +
      "CN -> VN : " +
        cNodes2vNodes.mkString("\t[",",\n\t ","]\n") +
      "Roles : " +
        roles.mkString("\t[",",\n\t ","]\n")

  }

  implicit val showEdgesMap : scalaz.Show[EdgeMap] = scalaz.Show.shows[EdgeMap] {
    case EdgeMap ( userMap, usedMap, accessKindMap,
      contents, containers, superTypes, subTypes,
      typeMemberUses2typeUsesMap,
      typeUses2typeMemberUsesMap,
      parameters, definition, types) =>
    val builder = new StringBuilder(150)

    builder.append("used -> user\n\t")
    builder.append(userMap.toString)
    builder.append("\nuser -> used\n\t")
    builder.append(usedMap.toString)

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
    builder.append("\ntypes\n\t")
    builder.append( mkMapStringSortedByKey(types))

    builder.toString()
  }


  def mkMapStringSortedByFullName(g : DependencyGraph, m : Map[NodeId,DGNode]) : String = {
    m.toList.map{case (id, _) => (g.fullName(id), id)}.
      sortBy(_._1).mkString("\t[",",\n\t ","]\n")

  }

  implicit val nodeIndexCordBuilder : CordBuilder[NodeIndex] = {
    case (g, NodeIndex(_, cNodes, removedCnodes,
    vNodes, removedVnodes,
    cNodes2vNodes,
    roles)) =>
      "Concrete Nodes : " +
        mkMapStringSortedByFullName(g, cNodes) +
        "Removed Concrete Nodes : " +
        mkMapStringSortedByFullName(g, removedCnodes) +
        "Virtual Nodes : " +
        mkMapStringSortedByFullName(g, vNodes) +
        "Removed Virtual Nodes : " +
        mkMapStringSortedByFullName(g, removedVnodes) +
        "CN -> VN : " +
        cNodes2vNodes.mkString("\t[",",\n\t ","]\n") +
        "Roles : " +
        roles.mkString("\t[",",\n\t ","]\n")
  }
}
