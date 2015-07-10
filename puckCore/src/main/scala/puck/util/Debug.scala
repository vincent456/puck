package puck.util

import puck.graph._
import ShowDG._

import scalaz.Show

object Debug {


  def printEdgeSet(g : DependencyGraph, logger : PuckLogger, s : Set[DGUses])=
    s.foreach(e => logger.writeln(s"\t\t*${showDG[DGEdge](g).shows(e)}"))

  def logUsersOf(g : DependencyGraph, logger : PuckLogger, n : NodeId) = {

    logger.writeln(s"users of ${showDG[NodeId](g).shows(n)} :")

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
        logger.writeln(s"\t- user ${showDG[NodeId](g).shows(userId)}")
        printUser(userId)
    }

  }

  def logUsedBy(g : DependencyGraph, logger : PuckLogger, n : NodeId) = {
    logger.writeln(s"used by ${showDG[NodeId](g).shows(n)} :")

    g.usedBy(n).foreach{
      usedId =>
        g.kindType(usedId) match {
          case InstanceValueDecl =>
            logger.writeln(s"\t- used type member ${showDG[NodeId](g).shows(usedId)}")
            logger.writeln("\tType Uses are:")
            printEdgeSet(g, logger, g.typeUsesOf(n, usedId))
          case TypeDecl =>
            logger.writeln(s"\t- used type ${showDG[NodeId](g).shows(usedId)}")
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

  implicit val showNodeIndex = Show.shows[NodeIndex] {
    case NodeIndex(_, cNodes, removedCnodes,
    vNodes, removedVnodes,
    cNodes2vNodes) =>
      "Concrete Nodes : " +
        cNodes.mkString("\t[",",\n\t ","]\n") +
      "Removed Concrete Nodes : " +
        removedCnodes.mkString("\t[",",\n\t ","]\n") +
      "Virtual Nodes : " +
        vNodes.mkString("\t[",",\n\t ","]\n") +
      "Removed Virtual Nodes : " +
        removedVnodes.mkString("\t[",",\n\t ","]\n") +
      "CN -> VN : " +
        cNodes2vNodes.mkString("\t[",",\n\t ","]\n")

  }

}
