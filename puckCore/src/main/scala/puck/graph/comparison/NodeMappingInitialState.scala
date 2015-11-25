package puck.graph.comparison

import puck.graph._
import puck.graph.comparison.RecordingComparator._
import puck.graph.transformations._
import puck.util.{PuckLogger, PuckLog}

import puck.search.SearchState


import scala.collection.immutable.HashSet
import scalaz._, Scalaz._

object NodeTransfoStatus {
  def apply( i : Int) = i match {
    case 1 => Created
    case 0 => Neuter
    case -1 => Deleted
    case _ => throw new Error("Illegal node transfo status value !")
  }
}

sealed abstract class NodeTransfoStatus
case object Created extends NodeTransfoStatus
case object Deleted extends NodeTransfoStatus
case object Neuter extends NodeTransfoStatus

object NodeMappingInitialState{

  val discardedOp : Operation => Boolean = {
    case _: AbstractionOp => true
    case _: TypeChange => true
    case _: ChangeNodeName => true
    case _: Comment => true
    case _: TypeDependency => true
    case _ => false
  }

  def normalizeNodeTransfos(rootKind : NodeKind,
                             l : Recording,
                            init : Seq[Transformation] = Seq()) : (Map[NodeId, (Int, NodeKind)], Seq[Transformation])= {
    val (map, rl) = l.foldLeft( (Map[NodeId, (Int, NodeKind)](), init) ){
      case ((m,l1), Transformation(Regular, CNode(n))) =>
        val (i, _) = m.getOrElse(n.id, (0, rootKind) )
        (m + (n.id -> ((i + 1, n.kind))), l1)
      case ((m,l1), Transformation(Reverse, CNode(n))) =>
        val (i, _) = m.getOrElse(n.id, (0, rootKind))
        (m + (n.id -> ((i - 1, n.kind))), l1)

      case ((m,l1), Transformation(_, op))
        if discardedOp(op) => (m, l1)

      case ((m, l1), t : Transformation) => (m, t +: l1)
      case ((m,l1), _) => (m, l1)

    }

    (map, rl.reverse)
  }

  def switchNodes[Kind <: NodeKind, T]
          (nodeStatusesMap : Map[NodeId, (Int, Kind)], init : T)
          (add : (T, (NodeId, NodeKind)) => T) : (Int, T, Seq[NodeId], Set[NodeId]) = {
    nodeStatusesMap.foldLeft[(Int, T, Seq[NodeId], Set[NodeId])](0, init, List(), HashSet()) {
      case ((cpt, addAcc, rmAcc, neuterAcc), (node, (i, kind))) =>
        NodeTransfoStatus(i) match {
          case Created => (cpt + 1, add(addAcc, (node, kind)), rmAcc, neuterAcc)
          case Deleted => (cpt, addAcc, node +: rmAcc, neuterAcc)
          case Neuter => (cpt, addAcc, rmAcc, neuterAcc + node)
        }
    }
  }

  implicit val defaultVerbosity = (PuckLog.NoSpecialContext, PuckLog.Debug)
}


import NodeMappingInitialState._

object RecordingComparatorInitialState {

  def apply(initialTransfos: Seq[Transformation],
            graph1: DependencyGraph,
            graph2: DependencyGraph,
            logger: PuckLogger): SearchState[Compared] = {

    logger.writeln("*********************************** ")
    logger.writeln("*********************************** ")
    logger.writeln("*********************************** ")
    logger.writeln("untouched recording 1 : ")
    graph1.recording foreach { t => logger.writeln(t.toString) }
    logger.writeln("*********************************** ")
    logger.writeln("*********************************** ")

    logger.writeln("untouched recording 2 : ")
    graph2.recording foreach { t => logger.writeln(t.toString) }
    logger.writeln("*********************************** ")
    logger.writeln("*********************************** ")

    val (nodeStatuses, remainingTransfos1) = normalizeNodeTransfos(graph1.rootKind, graph1.recording, initialTransfos)

    val (nodeStatuses2, remainingTransfos2) = normalizeNodeTransfos(graph2.rootKind, graph2.recording, initialTransfos)

    val (numCreatedNodes, initialMapping, removedNode, neuterNodes) =
      switchNodes(nodeStatuses, ResMap()) {
        case (m, (n, k0)) => m + (n -> Left(k0))
      }

    val (numCreatedNodes2, nodesToMap, otherRemovedNodes, otherNeuterNodes) =
      switchNodes(nodeStatuses2, NodesToMap()) {
        case (m, (n, k0)) =>
          val s = m getOrElse(k0, Seq[NodeId]())
          m + (k0 -> (n +: s))
      }


    val loggedResult = initialMapping.set("")
    var triedAll0 = false

    def writelnNode(graph: DependencyGraph)(nid: NodeId): Unit = {
      logger.writeln(s"$nid = ${graph.getNode(nid)}")
    }




    if (numCreatedNodes != numCreatedNodes2 ||
      !(removedNode forall otherRemovedNodes.contains)) {
      val sameNumberOfNodesToMap = numCreatedNodes == numCreatedNodes2
      val sameRemovedNodes = removedNode forall otherRemovedNodes.contains
      logger.writeln("sameNumberOfNodesToMap  = " + sameNumberOfNodesToMap)

      logger.writeln("same number of removed nodes = " + (removedNode.length == otherRemovedNodes.length))
      logger.writeln("same removed nodes = " + sameRemovedNodes)


      logger.writeln(s"initialMapping : $initialMapping, ${remainingTransfos1.size} remaining transfo")

      logger.writeln("created nodes :")
      initialMapping.keys foreach writelnNode(graph1)
      logger.writeln("neuter nodes : ")
      neuterNodes foreach writelnNode(graph1)

      logger.writeln("*******************************************************")
      logger.writeln("")
      logger.writeln("")

      logger.writeln("nodes to map : %s, %d remaining transfo".format(nodesToMap, remainingTransfos2.size))

      logger.writeln("created nodes :")
      nodesToMap foreach { case (_, s) => s foreach writelnNode(graph2) }
      logger.writeln("neuter nodes : ")
      otherNeuterNodes foreach writelnNode(graph2)

      logger.writeln("recording1")
      graph1.recording foreach { t => logger.writeln(t.toString) }

      logger.writeln("*******************************************************")
      logger.writeln("")
      logger.writeln("")

      logger.writeln("recording2")
      graph2.recording foreach { t => logger.writeln(t.toString) }

      new SearchState(0, None, LoggedError(NoSolution), Seq())
    }
    else {


      logger.writeln(s"initialMapping : $initialMapping, ${remainingTransfos1.size} remaining transfo")

      logger.writeln("created nodes :")
      initialMapping.keys foreach writelnNode(graph1)
      logger.writeln("neuter nodes : ")
      neuterNodes foreach writelnNode(graph1)

      logger.writeln("")
      logger.writeln("")
      remainingTransfos1 foreach { t => logger.writeln(t.toString) }

      logger.writeln("*******************************************************")
      logger.writeln("***************** filtering 1 *************************")
      logger.writeln("*******************************************************")

      val filteredTransfos1 = if (neuterNodes.nonEmpty) FilterNoise(remainingTransfos1, logger)
      else remainingTransfos1

      logger.writeln("*******************************************************")
      logger.writeln("*******************************************************")

      filteredTransfos1 foreach { t => logger.writeln(t.toString) }

      logger.writeln("")
      logger.writeln("")
      logger.writeln("*******************************************************")
      logger.writeln("*******************************************************")
      logger.writeln("************************* and *************************")
      logger.writeln("*******************************************************")
      logger.writeln("*******************************************************")
      logger.writeln("")
      logger.writeln("")

      logger.writeln("nodes to map : %s, %d remaining transfo".format(nodesToMap, remainingTransfos2.size, logger))

      logger.writeln("created nodes :")
      nodesToMap foreach { case (_, s) => s foreach writelnNode(graph2) }
      logger.writeln("neuter nodes : ")
      otherNeuterNodes foreach writelnNode(graph2)

      logger.writeln("")
      logger.writeln("")
      remainingTransfos2 foreach { t => logger.writeln(t.toString) }


      logger.writeln("*******************************************************")
      logger.writeln("***************** filtering 2 *************************")
      logger.writeln("*******************************************************")

      val filteredTransfos2 = if (otherNeuterNodes.nonEmpty) FilterNoise(remainingTransfos2, logger)
      else remainingTransfos2

      logger.writeln("*******************************************************")
      logger.writeln("*******************************************************")

      filteredTransfos2 foreach { t => logger.writeln(t.toString) }

      if (filteredTransfos1.length != filteredTransfos2.length)
        new SearchState(0, None, LoggedError(NoSolution), Seq())
      else{
        val res = (initialMapping, nodesToMap, filteredTransfos1, filteredTransfos2)
        new SearchState(0, None, LoggedSuccess(res), RecordingComparator.nextStates(res))
      }
    }


  }

}
