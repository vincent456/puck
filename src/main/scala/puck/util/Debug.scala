package puck.util

import puck.graph.ShowDG._
import puck.graph._
import puck.javaGraph.nodeKind.Primitive

/**
 * Created by lorilan on 4/9/15.
 */
object Debug {

  type EdgeT = (NodeId, NodeId)

  def printEdgeSet(g : DependencyGraph, s : Set[EdgeT])=
    s.foreach(e => g.logger.writeln(s"\t\t*${showDG[DGEdge](g).shows(DGEdge.uses(e))}"))

  def logUsersOf(g : DependencyGraph, n : NodeId) = {
    import g.logger

    logger.writeln(s"users of ${showDG[NodeId](g).shows(n)} :")

    def printUser = g.kindType(n) match {
      case TypeMember =>
        (userId : NodeId) =>
          logger.writeln("\tType Uses are:")
          printEdgeSet(g, g.typeUsesOf((userId,n)))
      case TypeDecl =>
        (userId : NodeId) =>
          logger.writeln("\tTypeMember Uses are:")
          printEdgeSet(g, g.typeMemberUsesOf((userId,n)))
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

  def logUsedBy(g : DependencyGraph, n : NodeId) = {
    import g.logger
    logger.writeln(s"used by ${showDG[NodeId](g).shows(n)} :")

    g.usedBy(n).foreach{
      usedId =>
        g.kindType(usedId) match {
          case TypeMember =>
            logger.writeln(s"\t- used type member ${showDG[NodeId](g).shows(usedId)}")
            logger.writeln("\tType Uses are:")
            printEdgeSet(g, g.typeUsesOf((n, usedId)))
          case TypeDecl =>
            logger.writeln(s"\t- used type ${showDG[NodeId](g).shows(usedId)}")
            logger.writeln("\tTypeMember Uses are:")
            printEdgeSet(g, g.typeMemberUsesOf((n, usedId)))
          case kt => logger.writeln(s"$kt (${g.getNode(usedId).kind}) unhandled")

        }

    }
  }

  def logUsesDependency(g : DependencyGraph, n : NodeId) = {
    logUsersOf(g, n)
    logUsedBy(g, n)

  }

}
