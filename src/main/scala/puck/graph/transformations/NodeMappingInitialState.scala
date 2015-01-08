package puck.graph
package transformations

import MappingChoices.{ResMap, NodesToMap}

import puck.javaAG.nodeKind.JavaRoot
import puck.search.SearchState
import puck.util.{PuckLog, PuckLogger}

import scala.collection.immutable.HashSet
import scala.util.{Failure, Try}

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

/**
 * Created by lorilan on 30/09/14.
 */

object NodeMappingInitialState{

  def normalizeNodeTransfos(l : Seq[Transformation],
                            init : Seq[Transformation] = Seq()) : (Map[NodeId, (Int, NodeKind)], Seq[Transformation])= {
    val (map, rl) = l.foldLeft( (Map[NodeId, (Int, NodeKind)](), init) ){
      case ((m,l1), Transformation(Add, TTNode(id, _, kind, _, _, _ ))) =>
        val (i, _) = m.getOrElse(id, (0, JavaRoot) )
        (m + (id -> (i + 1, kind)), l1)
      case ((m,l1), Transformation(Remove, TTNode(id, _, kind, _, _, _ ))) =>
        val (i, _) = m.getOrElse(id, (0, JavaRoot))
        (m + (id -> (i - 1, kind)), l1)

      //removing the dependendency and abstraction of the comparison
      // they are used to compute the change on the graph, its the change themselves we want to compare
      //if added back think to uncommment comparison in RecordingComparator.compare
      /*case ((m,l1), Transformation(_, TTDependency(_, _))) => (m, l1)
      case ((m,l1), Transformation(_, TTConstraint(_,_))) => (m, l1)*/
      case ((m,l1), Transformation(_, TTAbstraction(_, _, _))) => (m, l1)
      case ((m,l1), Transformation(_, TTTypeRedirection(_,_,_, _))) => (m, l1)
      case ((m,l1), t : Transformation) => (m, t +: l1)
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

  def select[T](l : List[T], pred : T => Boolean) : (Option[T], List[T]) = {
    def aux(l1 : List[T], acc : List[T]) : (Option[T], List[T]) =
      if(l1.isEmpty) (None, l)
      else if(pred(l1.head)) (Some(l1.head), acc reverse_::: l1.tail)
      else aux(l1.tail, l1.head :: acc)

    aux(l, List[T]())
  }


  implicit val defaultVerbosity = (PuckLog.GraphComparisonSearch, PuckLog.Debug)

  def filterNoise[Kind <: NodeKind, T](transfos : Seq[Transformation], logger : PuckLogger):
  Seq[Transformation] = {

    def printRule(name : => String, op1 : => String, op2 : => String, res : => String ){
      logger.writeln(name +" : ")
      logger.writeln(op1)
      logger.writeln("+ " + op2)
      logger.writeln("= " + res)
      logger.writeln("----------------------------------------------")
    }

    def mapUntil( stoppingEdge : AGEdge,
                  transfos : List[Transformation])
                (rule : (Transformation => Transformation)): Seq[Transformation] = {

      def aux(acc : Seq[Transformation], l : Seq[Transformation]) : Seq[Transformation] ={
        if(l.isEmpty) acc.reverse
        else
          l.head match {
            case Transformation(_, TTRedirection(`stoppingEdge`, _)) =>
              RecordingComparator.revAppend(acc, l)
            case _ => aux(rule(l.head) +: acc, l.tail)

          }
      }
      aux(List(), transfos)
    }

    // We are going backwards !
    def aux(filteredTransfos : Seq[Transformation],
            l : Seq[Transformation],
             removedEdges : Seq[AGEdge]): (Seq[Transformation], Seq[AGEdge]) = {
      l match {
        case List() => (filteredTransfos, removedEdges)
        case (op2 @ Transformation(Remove, TTEdge(AGEdge(Contains, n1, n2)))) :: tl =>
          /*val (applied, newTl) = applyRuleOnce[Transformation](tl)
          { case Transformation(Add(), TTEdge(AGEdge(Contains(), `n1`, `n2`))) => (true, None)
          case _ => (false, None)
          }
          if(applied) aux(filteredTransfos, newTl)
          else aux(l.head :: filteredTransfos, l)*/
          select[Transformation](tl,
          { case Transformation(Add, TTEdge(AGEdge(Contains, `n1`, `n2`))) => true
          case _ => false
          }) match {
            case (Some( op1 ), newTl) =>
              printRule("AddDel", op1.toString, op2.toString, "None")
              aux(filteredTransfos, newTl, removedEdges)
            case (None, _) => aux(l.head +: filteredTransfos, l.tail, removedEdges)
          }
        case (op1 @ Transformation(Add, TTRedirection(stopingEdge @ AGEdge(kind, n1, n2), Source(n3)))) :: tl =>
          aux(filteredTransfos, mapUntil(stopingEdge, tl)
          { case op2 @ Transformation(Add, TTRedirection(AGEdge(`kind`, n0, `n2`), Source(`n1`))) =>
            val res = Transformation(Add, TTRedirection(AGEdge(kind, n0, n2), Source(n3)))
            printRule("RedRed_src", op2.toString, op1.toString, res.toString)
            res
          case op2 @ Transformation(Add, TTEdge(AGEdge(`kind`, `n1`, `n2`))) =>
            val res = Transformation(Add, TTEdge(AGEdge(kind, n3, n2)))
            printRule("AddRed_src", op2.toString, op1.toString, res.toString)
            res
          case t => t
          }, removedEdges)

        case (op1 @ Transformation(Add, TTRedirection(stopingEdge @ AGEdge(kind, n1, n2), Target(n3)))) :: tl =>
          aux(filteredTransfos, mapUntil(stopingEdge, tl)
          { case op2 @ Transformation(Add, TTRedirection(AGEdge(`kind`, `n1`, n0), Target(`n2`))) =>
            val res = Transformation(Add, TTRedirection(AGEdge(kind, n1, n0), Target(n3)))
            printRule("RedRed_tgt", op2.toString, op1.toString, res.toString)
            res
          case op2 @ Transformation(Add, TTEdge(AGEdge(`kind`, `n1`, `n2`))) =>
            val res = Transformation(Add, TTEdge(AGEdge(kind, n1, n3)))
            printRule("AddRed_tgt", op2.toString, op1.toString, res.toString)
            res
          case t => t
          }, removedEdges)

        case (t @ Transformation(Add, TTEdge(_))):: tl => aux(t +: filteredTransfos, tl, removedEdges)
        case (Transformation(Remove, TTEdge(e))):: tl => aux(filteredTransfos, tl, e +: removedEdges)

        case hd :: _ => sys.error(hd  + " should not be in list")
      }
    }

    val (normalTransfos, removedEdges) = aux(Seq(), transfos, Seq())

    removedEdges.foldLeft(normalTransfos){case (normalTransfos0, e) =>
      normalTransfos0 filter {
        case Transformation(Add, TTEdge(`e`)) => false
        case _ => true
      }
    }

  }
}

class NodeMappingInitialState
( initialTransfos : Seq[Transformation],
  val engine : RecordingComparator,
  graph1 : AccessGraph,
  graph2 : AccessGraph,
  k: Try[ResMap] => Unit,
  logger : PuckLogger)
  extends SearchState[ResMap]{
  //extends NodeMappingState(0, eng, null, null, None) {
  val id = 0
  val prevState = None

  import puck.graph.transformations.NodeMappingInitialState._


  val (nodeStatuses, remainingTransfos1) = normalizeNodeTransfos(graph1.recording(), initialTransfos)

  val (nodeStatuses2, remainingTransfos2) = normalizeNodeTransfos(graph2.recording(), initialTransfos)

  val (numCreatedNodes, initialMapping, removedNode, neuterNodes) =
    switchNodes(nodeStatuses, ResMap()){
      case (m, (n, k0)) => m + (n -> (k0, None))
    }

  val (numCreatedNodes2, nodesToMap, otherRemovedNodes, otherNeuterNodes) =
    switchNodes(nodeStatuses2, NodesToMap()){
      case (m, (n, k0)) =>
        val s = m getOrElse (k0, Seq[NodeId]())
        m + (k0 -> (n +: s))
    }


  val result = initialMapping
  var triedAll0 = false

  override def triedAll = triedAll0

  import puck.graph.transformations.NodeMappingInitialState.defaultVerbosity

  def printlnNode(graph: AccessGraph)( nid : NodeId){
    val n = graph.getNode(nid)
    logger.writeln("%d = %s(%s)".format(n.id, n.kind, n.fullName))
  }


  override def executeNextChoice() {
    triedAll0 = true

    if(numCreatedNodes != numCreatedNodes2 ||
      !(removedNode forall otherRemovedNodes.contains)) {
      val sameNumberOfNodesToMap = numCreatedNodes == numCreatedNodes2
      val sameRemovedNodes = removedNode forall otherRemovedNodes.contains
      logger.writeln("sameNumberOfNodesToMap  = " + sameNumberOfNodesToMap)

      logger.writeln("same number of removed nodes = " + (removedNode.length == otherRemovedNodes.length))
      logger.writeln("same removed nodes = " + sameRemovedNodes)


      logger.writeln(s"initialMapping : $initialMapping, ${remainingTransfos1.size} remaining transfo")

      logger.writeln("created nodes :")
      initialMapping.keys foreach printlnNode(graph1)
      logger.writeln("neuter nodes : ")
      neuterNodes foreach printlnNode(graph1)

      logger.writeln("*******************************************************")
      logger.writeln("")
      logger.writeln("")

      logger.writeln("nodes to map : %s, %d remaining transfo".format(nodesToMap, remainingTransfos2.size))

      logger.writeln("created nodes :")
      nodesToMap foreach {case (_, s) => s foreach printlnNode(graph2)}
      logger.writeln("neuter nodes : ")
      otherNeuterNodes foreach printlnNode(graph2)

      logger.writeln("recording1")
      graph1.recording() foreach { t => logger.writeln(t.toString)}

      logger.writeln("*******************************************************")
      logger.writeln("")
      logger.writeln("")

      logger.writeln("recording2")
      graph2.recording() foreach { t => logger.writeln(t.toString)}

      k(Failure(NoSolution))
    }
    else {


      logger.writeln(s"initialMapping : $initialMapping, ${remainingTransfos1.size} remaining transfo")

      logger.writeln("created nodes :")
      initialMapping.keys foreach printlnNode(graph1)
      logger.writeln("neuter nodes : ")
      neuterNodes foreach printlnNode(graph1)

      logger.writeln("")
      logger.writeln("")
      remainingTransfos1 foreach { t => logger.writeln(t.toString)}

      logger.writeln("*******************************************************")
      logger.writeln("***************** filtering 1 *************************")
      logger.writeln("*******************************************************")

      val filteredTransfos1 = if(neuterNodes.nonEmpty) filterNoise(remainingTransfos1, logger)
      else remainingTransfos1

      logger.writeln("*******************************************************")
      logger.writeln("*******************************************************")

      filteredTransfos1 foreach { t => logger.writeln(t.toString)}

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
      nodesToMap foreach {case (_, s) => s foreach printlnNode(graph2)}
      logger.writeln("neuter nodes : ")
      otherNeuterNodes foreach printlnNode(graph2)

      logger.writeln("")
      logger.writeln("")
      remainingTransfos2 foreach { t => logger.writeln(t.toString)}


      logger.writeln("*******************************************************")
      logger.writeln("***************** filtering 2 *************************")
      logger.writeln("*******************************************************")

      val filteredTransfos2 = if(otherNeuterNodes.nonEmpty) filterNoise(remainingTransfos2, logger)
      else remainingTransfos2

      logger.writeln("*******************************************************")
      logger.writeln("*******************************************************")

      filteredTransfos2 foreach { t => logger.writeln(t.toString)}

      if(filteredTransfos1.length != filteredTransfos2.length)
        k(Failure(NoSolution))

      engine.compare(filteredTransfos1, filteredTransfos2, initialMapping, nodesToMap, k)
      /*eng.compare(filteredTransfos1, filteredTransfos2,
        neuterNodes.foldLeft(initialMapping){ (m, n) => m + (n -> None) },
        nodesToMap ++ otherNeuterNodes)*/
      //eng.compare(remainingTransfos1, remainingTransfos2, initialMapping, nodesToMap)

    }


  }
}



