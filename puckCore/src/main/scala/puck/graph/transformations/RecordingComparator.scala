package puck.graph
package transformations

import puck.PuckError
import puck.graph.transformations.MappingChoices.{Kargs, NodesToMap, ResMap}
import puck.search.FindFirstSearchEngine
import puck.util.{PuckLogger, PuckNoopLogger}

import scala.collection.mutable
import scalaz._, Scalaz._

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

object NoSolution extends PuckError("No solution")
object WrongMapping extends PuckError("Wrong mapping")

class RecordingComparator
( initialTransfos : Seq[Transformation],
  graph1 : DependencyGraph,
  graph2 : DependencyGraph,
  logger : PuckLogger = PuckNoopLogger) {

  val engine : FindFirstSearchEngine[ResMap] =
    new FindFirstSearchEngine[ResMap](
      k => new NodeMappingInitialState(initialTransfos, this, graph1, graph2, k, logger)
    )

  def attribNode(node : NodeId,
                 map : ResMap,
                 nodesToMap : NodesToMap)
                (k : Kargs => Unit) : Unit = {

    map.getOrElse(node, (graph1.getConcreteNode(node).kind, Some(node))) match {
      case (_, Some(n)) =>
        engine.newCurrentState(map.set(""), new StackSaver(k, n, nodesToMap))

      case (kind, None) =>
        nodesToMap.getOrElse(kind, Seq()) match {
          case Seq() => k((node, LoggedError(NoSolution), nodesToMap))
          case l =>
            val choices =
              new MappingChoices(k, node, kind, nodesToMap,
              mutable.Stack[NodeId]().pushAll(l),
              mutable.Stack[NodeId]())

            engine.newCurrentState(map.set(""), choices)
        }
    }
  }

  def attribNode(nodes : Seq[NodeId],
                 map : ResMap,
                 nodesToMap : NodesToMap)
                (k : (Seq[NodeId], LoggedTry[ResMap], NodesToMap) => Unit) : Unit = {


    def aux(nodes0 : Seq[NodeId], nodes1: Seq[NodeId])(kargs : Kargs) : Unit = {
      kargs match {
        case (node, map0, nodesToMap0) =>
          (nodes0, map0.value) match {
            case (Seq(), _) =>
              val l = (node +: nodes1).reverse.tail // we do not forget the last node and we drop the first dummy value
              k(l, map0, nodesToMap0)
            case (_, \/-(m)) =>
                attribNode(nodes0.head, m, nodesToMap0){aux(nodes0.tail, node +: nodes1)}
            case (_, -\/(_)) => k(nodes, map0, nodesToMap)
          }
      }
    }
    aux(nodes, Seq[NodeId]())((DependencyGraph.dummyId, LoggedSuccess(map), nodesToMap)) //null will be dropped in aux
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

  private [this] var computed = false
  private [this] var result = false
  def compare() : Boolean = {
    if(! computed) {
      engine.explore()
      result = engine.successes.nonEmpty
      computed = true
    }
    result
  }

  def compare(ts1 : Seq[Transformation],
              ts2 : Seq[Transformation],
              map : ResMap, nodesToMap : NodesToMap,
              k : LoggedTry[ResMap] => Unit) : Unit = {
    if (ts1.isEmpty && ts2.isEmpty)
      k(LoggedSuccess(map))
    else {
      def removeFirstAndCompareNext(tgt : Operation,
                                    tryMap : LoggedTry[ResMap], nodesToMap : NodesToMap) =
        tryMap.value match {
          case -\/(_) => k(tryMap)
          case \/-(m) =>
            removeFirst(ts2, ts1.head.direction, tgt) match {
            case None => k(LoggedError(WrongMapping))
//              println("Failure on mapping : ")
//              println(map.mkString("\t", "\n\t", "\n"))
//              println(ts1.head + " mapped as ")
//              println(Transformation(ts1.head.direction, tgt) + "cannot be found in :")
//              println(ts2.mkString("\t", "\n\t", "\n"))
//              println("**************************************")
            case Some(newTs2) =>
              //println("success")
              compare(ts1.tail, newTs2, m, nodesToMap, k)
          }
      }

      ts1.head.operation match {
        case Edge(e) => attribNode(Seq(e.source, e.target), map, nodesToMap) {
          case (Seq(src, tgt), map1, nodesToMap1) =>
            removeFirstAndCompareNext(Edge(e.copy(src, tgt)), map1, nodesToMap1 )
        }

        case RedirectionOp(e, extremity) =>
          attribNode(Seq(e.source, e.target, extremity.node), map, nodesToMap){
            case (Seq(src, tgt, newExtyNode), map1, nodesToMap1) =>
              removeFirstAndCompareNext(RedirectionOp(
                e.kind(src, tgt), extremity.create(newExtyNode)), map1, nodesToMap1)
          }


        //case TTNode(_) => throw new Error("should not happen !!")
        //removing the dependendency and abstraction of the comparison
        // they are used to compute the change on the graph, its the change themselves we want to compare
        // removed in NodeMappingInitialState.normalizeNodeTransfos
        case _ : TypeRedirection // TODO see if need to be compared
             | _ : TypeDependency
             | _ : AbstractionOp
             | _ : VNode
             | _ : CNode
             | _ : ChangeNodeName
             | _ : Comment => throw new Error("should not happen !!")

      }
    }
  }
}
