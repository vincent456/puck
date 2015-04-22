package puck.graph.transformations

import puck.PuckError
import puck.graph.{Try, NodeId, DependencyGraph, DGEdge}
import puck.search.{FindFirstSearchEngine, SearchState}
import puck.util.{PuckLogger, PuckNoopLogger}

import scala.collection.mutable
import scalaz.{\/-, -\/}

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

object NoSolution extends PuckError("No solution")
object WrongMapping extends PuckError("Wrong mapping")

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
                (k : Kargs => Unit) : Unit = {

    map.getOrElse(node, (graph1.getConcreteNode(node).kind, Some(node))) match {
      case (_, Some(n)) =>
        newCurrentState(map, new StackSaver(k, n, nodesToMap))

      case (kind, None) =>
        nodesToMap.getOrElse(kind, Seq()) match {
          case Seq() => k((node, -\/(NoSolution), nodesToMap))
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
                (k : (Seq[NodeId], Try[ResMap], NodesToMap) => Unit) : Unit = {


    def aux(nodes0 : Seq[NodeId], nodes1: Seq[NodeId])(kargs : Kargs) : Unit = {
      kargs match {
        case (node, map0, nodesToMap0) =>
          (nodes0, map0) match {
            case (Seq(), _) =>
              val l = (node +: nodes1).reverse.tail // we do not forget the last node and we drop the first dummy value
              k(l, map0, nodesToMap0)
            case (_, \/-(m)) =>
                attribNode(nodes0.head, m, nodesToMap0){aux(nodes0.tail, node +: nodes1)}
            case (_, -\/(_)) => k(nodes, map0, nodesToMap)
          }
      }
    }
    aux(nodes, Seq[NodeId]())((DependencyGraph.dummyId, \/-(map), nodesToMap)) //null will be dropped in aux
  }



  def removeFirst(l : Seq[Transformation],
                  op : Direction,
                  tgt : Operation) :  Option[Seq[Transformation]] = {

    RecordingComparator.removeFirst(l, {(t : Transformation) => t.direction == op && t.operation == tgt})
  }

  /*def printAssociatedChoices(map : ResMap) = {
    map.foreach{
      case (n, (_, Some(mapping))) => println("*  %s ==> %s".format(n, mapping))
      case _ => ()
    }
  }
*/
  def compare(ts1 : Seq[Transformation],
              ts2 : Seq[Transformation],
              map : ResMap, nodesToMap : NodesToMap,
              k : Try[ResMap] => Unit) : Unit = {
    if (ts1.isEmpty && ts2.isEmpty)
      k(\/-(map))
    else {
      def removeFirstAndCompareNext(tgt : Operation,
                                    tryMap : Try[ResMap], nodesToMap : NodesToMap) =
        tryMap match {
          case -\/(_) => k(tryMap)
          case \/-(m) =>
            removeFirst(ts2, ts1.head.direction, tgt) match {
            case None => k(-\/(WrongMapping))
             /* println("Failure on mapping : ")
              println(map.mkString("\t", "\n\t", "\n"))
              println(ts1.head + " mapped as ")
              println(Transformation(ts1.head.operation, tgt) + "cannot be found in :")
              println(ts2.mkString("\t", "\n\t", "\n"))
              println("**************************************")*/
            case Some(newTs2) =>
              //println("success")
              compare(ts1.tail, newTs2, m, nodesToMap, k)
          }
      }

      ts1.head.operation match {
        case Edge(e) => attribNode(Seq(e.source, e.target), map, nodesToMap) {
          case (Seq(src, tgt), map1, nodesToMap1) =>
            removeFirstAndCompareNext(Edge(DGEdge(e.kind, src, tgt)), map1, nodesToMap1 )
        }

        case RedirectionOp(e, extremity) =>
          attribNode(Seq(e.source, e.target, extremity.node), map, nodesToMap){
            case (Seq(src, tgt, newExtyNode), map1, nodesToMap1) =>
              removeFirstAndCompareNext(RedirectionOp(
                DGEdge(e.kind, src, tgt), extremity.create(newExtyNode)), map1, nodesToMap1)
          }


        //case TTNode(_) => throw new Error("should not happen !!")
        //removing the dependendency and abstraction of the comparison
        // they are used to compute the change on the graph, its the change themselves we want to compare
        // removed in NodeMappingInitialState.normalizeNodeTransfos
        case _ : TypeRedirection // TODO see if need to be compared
             | _ : TypeDependency
             | _ : Abstraction
             | _ : VNode
             | _ : CNode
             | _ : ChangeNodeName
             | _ : Comment => throw new Error("should not happen !!")

      }
    }
  }
}
