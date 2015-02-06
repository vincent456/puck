package puck.graph.transformations

import puck.graph.{NodeId, DependencyGraph, DGEdge}
import puck.search.{FindFirstSearchEngine, SearchState}
import puck.util.{PuckLogger, PuckNoopLogger}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * Created by lorilan on 07/07/14.
 */

object RecordingComparator{

  def revAppend[T] (heads : Seq[T], tails : Seq[T]) : Seq[T] = {
     def aux(hds: Seq[T], tls : Seq[T]) : Seq[T] =
      if(hds.isEmpty) tls
      else aux(hds.tail, hds.head +: tls)

     aux(heads, tails)
  }

  def removeFirst[T](l : Seq[T], pred : T => Boolean) : Option[Seq[T]] = {
    def aux(l1 : Seq[T], acc : Seq[T]) : Option[Seq[T]] =
      if(l1.isEmpty) None
      else if(pred(l1.head)) Some(revAppend(acc, l1.tail))
      else aux(l1.tail, l1.head +: acc)


    aux(l, Seq[T]())
  }
}

import puck.graph.transformations.MappingChoices.{Kargs, NodesToMap, ResMap}

object NoSolution extends Throwable
object WrongMapping extends Throwable

class RecordingComparator
( initialTransfos : Seq[Transformation],
  graph1 : DependencyGraph,
  graph2 : DependencyGraph,
  logger : PuckLogger = PuckNoopLogger)
  extends FindFirstSearchEngine[ResMap] {

  def createInitialState(k: Try[ResMap] => Unit): SearchState[ResMap] =
    new NodeMappingInitialState(initialTransfos, this, graph1, graph2, k, logger)


  def attribNode(node : NodeId,
                 map : ResMap,
                 nodesToMap : NodesToMap)
                (k : Kargs => Unit) {

    map.getOrElse(node, (graph1.getNode(node).kind, Some(node))) match {
      case (_, Some(n)) => k(n, Success(map), nodesToMap)

      case (kind, None) =>
        nodesToMap.getOrElse(kind, Seq()) match {
          case Seq() => k(node, Failure(NoSolution), nodesToMap)
          case l =>
            val choices =
              new MappingChoices(k, node, kind, nodesToMap,
              mutable.Stack[NodeId]().pushAll(l),
              mutable.Stack[NodeId]())

            newCurrentState(map, choices)
        }
    }
  }

  def attribNode(nodes : Seq[NodeId],
                 map : ResMap,
                 nodesToMap : NodesToMap)
                (k : (Seq[NodeId], Try[ResMap], NodesToMap) => Unit) {


    def aux(nodes0 : Seq[NodeId], nodes1: Seq[NodeId])(kargs : Kargs){
      kargs match {
        case (node, map0, nodesToMap0) =>
          nodes0 match {
            case Seq() =>
              val l = (node +: nodes1).reverse.tail // we do not forget the last node and we drop the first dummy value
              k(l, map0, nodesToMap0)
            case _ =>
              if(map0.isSuccess)
                attribNode(nodes0.head, map0.get, nodesToMap0){aux(nodes0.tail, node +: nodes1)}
              else
                k(nodes, map0, nodesToMap)
          }
      }
    }
    aux(nodes, Seq[NodeId]())((DependencyGraph.dummyId, Success(map), nodesToMap)) //null will be dropped in aux
  }



  def removeFirst(l : Seq[Transformation],
                  op : Operation,
                  tgt : TransformationTarget) :  Option[Seq[Transformation]] = {

    RecordingComparator.removeFirst(l, {(t : Transformation) => t.operation == op && t.target == tgt})
  }


  def printAssociatedChoices(map : ResMap) = {
    map.foreach{
      case (n, (_, Some(mapping))) => println("*  %s ==> %s".format(n, mapping))
      case _ => ()
    }
  }

  def compare(ts1 : Seq[Transformation],
              ts2 : Seq[Transformation],
              map : ResMap, nodesToMap : NodesToMap,
              k : Try[ResMap] => Unit){
    if (ts1.isEmpty && ts2.isEmpty)
      k(Success(map))
    else {
      def removeFirstAndCompareNext(tgt : TransformationTarget,
                                    tryMap : Try[ResMap], nodesToMap : NodesToMap){
        if(tryMap.isFailure) k(tryMap)
        else removeFirst(ts2, ts1.head.operation, tgt) match {
          case None => k(Failure(WrongMapping))
           /* println("Failure on mapping : ")
            println(map.mkString("\t", "\n\t", "\n"))
            println(ts1.head + " mapped as ")
            println(Transformation(ts1.head.operation, tgt) + "cannot be found in :")
            println(ts2.mkString("\t", "\n\t", "\n"))
            println("**************************************")
*/
          case Some(newTs2) =>
            //println("success")
            compare(ts1.tail, newTs2, tryMap.get, nodesToMap, k)
        }
      }

      ts1.head.target match {
        case TTEdge(e) => attribNode(Seq(e.source, e.target), map, nodesToMap) {
          case (Seq(src, tgt), map1, nodesToMap1) =>
            removeFirstAndCompareNext(TTEdge(DGEdge(e.kind, src, tgt)), map1, nodesToMap1 )
        }

        case TTRedirection(e, extremity) =>
          attribNode(Seq(e.source, e.target, extremity.node), map, nodesToMap){
            case (Seq(src, tgt, newExtyNode), map1, nodesToMap1) =>
              removeFirstAndCompareNext(TTRedirection(
                DGEdge(e.kind, src, tgt), extremity.create(newExtyNode)), map1, nodesToMap1)
          }


        //case TTNode(_) => throw new Error("should not happen !!")
        //removing the dependendency and abstraction of the comparison
        // they are used to compute the change on the graph, its the change themselves we want to compare
        // removed in NodeMappingInitialState.normalizeNodeTransfos
        case TTTypeRedirection(_, _, _, _) // TODO see if need to be compared
             | TTAbstraction(_, _, _)
             | TTNode(_, _, _, _, _) => throw new Error("should not happen !!")

      }
    }
  }
}
