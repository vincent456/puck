package puck.graph.immutable.transformations

import puck.graph.AGError
import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{AGEdge, AccessGraph, NodeKind}
import puck.search.{FindFirstSearchEngine, SearchState}
import puck.util.{PuckLogger, PuckNoopLogger}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * Created by lorilan on 07/07/14.
 */

object RecordingComparator{
  def removeFirst[T](l : Seq[T], pred : T => Boolean) : Option[Seq[T]] = {
    def aux(l1 : Seq[T], acc : Seq[T]) : Option[Seq[T]] =
      if(l1.isEmpty) None
      else if(pred(l1.head)) Some(acc.reverse ++: l1.tail)
      else aux(l1.tail, l1.head +: acc)


    aux(l, Seq[T]())
  }
}

import puck.graph.immutable.transformations.MappingChoices.{Kargs, NodesToMap, ResMapping}

class NoSolution extends Throwable

class RecordingComparator[Kind <: NodeKind[Kind], T]
( initialTransfos : Seq[Transformation[Kind, T]],
  graph1 : AccessGraph[Kind, T],
  graph2 : AccessGraph[Kind, T],
  logger : PuckLogger = PuckNoopLogger)
  extends FindFirstSearchEngine[ResMapping[Kind]] {

  def createInitialState(k: Try[ResMapping[Kind]] => Unit): SearchState[ResMapping[Kind]] =
     new NodeMappingInitialState(initialTransfos, this, graph1, graph2, k, logger)

  def attribNode(node : NodeId[Kind],
                 map : ResMapping[Kind],
                 nodesToMap : NodesToMap[Kind])
                (k : Kargs[Kind] => Unit) {

    map.getOrElse(node, Some(node)) match {
      case Some(n) => k(n, map, nodesToMap)

      case None =>
        nodesToMap match {
          case Seq() => throw new NoSolution()
          case l =>
            val (sameKind, others) = l partition {n => graph2.getNode(n).kind == graph1.getNode(node).kind}
            val choices = new MappingChoices(k, node, nodesToMap,
              mutable.Set[NodeId[Kind]]() ++ sameKind,
              mutable.Set[NodeId[Kind]]() ++ others)

            newCurrentState(map, choices)
        }
    }
  }

  def attribNode(nodes : Seq[NodeId[Kind]],
                 map : ResMapping[Kind],
                 nodesToMap : NodesToMap[Kind])
                (k : (Seq[NodeId[Kind]], ResMapping[Kind], NodesToMap[Kind]) => Unit) {


    def aux(nodes0 : Seq[NodeId[Kind]], nodes1: Seq[NodeId[Kind]])(kargs : Kargs[Kind]){
      kargs match {
        case (node, map0, nodesToMap0) =>
          nodes0 match {
            case Seq() =>
              val l = (node +: nodes1).reverse.tail // we do not forget the last node and we drop the first dummy value
              k(l, map0, nodesToMap0)
            case _ =>
              attribNode(nodes0.head, map0, nodesToMap0){aux(nodes0.tail, node +: nodes1)}
          }
      }
    }
    aux(nodes, Seq[NodeId[Kind]]())((AccessGraph.dummyId, map, nodesToMap)) //null will be dropped in aux
  }



  def removeFirst(l : Seq[Transformation[Kind, T]],
                  op : Operation,
                  tgt : TransformationTarget[Kind, T]) :  Option[Seq[Transformation[Kind, T]]] = {

    RecordingComparator.removeFirst(l, {(t : Transformation[Kind, T]) => t.operation == op && t.target == tgt})
  }


  def printAssociatedChoices(map : ResMapping[Kind]) = {
    map.foreach{
      case (n, Some(mapping)) => println("*  %s ==> %s".format(n, mapping))
      case _ => ()
    }
  }

  def compare(ts1 : Seq[Transformation[Kind, T]],
              ts2 : Seq[Transformation[Kind, T]],
              map : ResMapping[Kind], nodesToMap : NodesToMap[Kind],
              k : Try[ResMapping[Kind]] => Unit){
    if (ts1.isEmpty && ts2.isEmpty)
      k(Success(map))
    else {
      def removeFirstAndCompareNext(tgt : TransformationTarget[Kind, T],
                                    map : ResMapping[Kind], nodesToMap : NodesToMap[Kind]){
        removeFirst(ts2, ts1.head.operation, tgt) match {
          case None => k(Failure(new Error("wrong mapping")))
           /* println("Failure on mapping : ")
            println(map.mkString("\t", "\n\t", "\n"))
            println(ts1.head + " mapped as ")
            println(Transformation(ts1.head.operation, tgt) + "cannot be found in :")
            println(ts2.mkString("\t", "\n\t", "\n"))
            println("**************************************")
*/
          case Some(newTs2) =>
            //println("success")
            compare(ts1.tail, newTs2, map, nodesToMap, k)
        }
      }

      ts1.head.target match {
        case TTEdge(e) => attribNode(Seq(e.source, e.target), map, nodesToMap) {
          case (Seq(src, tgt), map1, nodesToMap1) =>
            removeFirstAndCompareNext(TTEdge(AGEdge(e.kind, src, tgt)), map1, nodesToMap1 )
        }

        case TTRedirection(e, extremity) =>
          attribNode(Seq(e.source, e.target, extremity.node), map, nodesToMap){
            case (Seq(src, tgt, newExtyNode), map1, nodesToMap1) =>
              removeFirstAndCompareNext(TTRedirection(
                AGEdge(e.kind, src, tgt), extremity.create(newExtyNode)), map1, nodesToMap1)
          }


        //case TTNode(_) => throw new Error("should not happen !!")
        //removing the dependendency and abstraction of the comparison
        // they are used to compute the change on the graph, its the change themselves we want to compare
        // removed in NodeMappingInitialState.normalizeNodeTransfos
        case TTTypeRedirection(_, _, _, _) // TODO see if need to be compared
             | TTNode(_, _, _, _, _, _) => throw new Error("should not happen !!")

      }
    }
  }
}
