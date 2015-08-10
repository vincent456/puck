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

  implicit val showNodeIndex = scalaz.Show.shows[NodeIndex] {
    case NodeIndex(_, cNodes, removedCnodes,
    vNodes, removedVnodes,
    cNodes2vNodes) =>
      "Concrete Nodes : " +
        mkMapStringSortedByKey(cNodes) +
      "Removed Concrete Nodes : " +
        mkMapStringSortedByKey(removedCnodes) +
      "Virtual Nodes : " +
        mkMapStringSortedByKey(vNodes) +
      "Removed Virtual Nodes : " +
        mkMapStringSortedByKey(removedVnodes) +
      "CN -> VN : " +
        cNodes2vNodes.mkString("\t[",",\n\t ","]\n")

  }

}
