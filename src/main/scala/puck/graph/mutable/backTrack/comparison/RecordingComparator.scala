package puck.graph.mutable.backTrack.comparison

import puck.graph.mutable.backTrack._
import puck.graph.mutable.{AGNode, NodeKind, AGEdge}
import puck.search.FindFirstSearchEngine
import puck.util.{PuckNoopLogger, PuckLogger}

import scala.collection.mutable

/**
 * Created by lorilan on 07/07/14.
 */

object RecordingComparator{
  def removeFirst[T](l : List[T], pred : T => Boolean) : Option[List[T]] = {
    def aux(l1 : List[T], acc : List[T]) : Option[List[T]] =
      if(l1.isEmpty) None
      else if(pred(l1.head)) Some(acc reverse_::: l1.tail)
      else aux(l1.tail, l1.head :: acc)


    aux(l, List[T]())
  }
}

import AssumedChoices.{Kargs, NodesToMap, ResMapping}

class NoSolution extends Throwable

class RecordingComparator[Kind <: NodeKind[Kind]](private [comparison] val initialTransfos : List[Transformation[Kind]],
                                                  recording1 : Recording[Kind],
                                                  recording2 : Recording[Kind],
                                                  logger : PuckLogger = PuckNoopLogger)
  extends FindFirstSearchEngine[ResMapping[Kind]] {

  def attribNode(node : AGNode[Kind],
                 map : ResMapping[Kind],
                 nodesToMap : NodesToMap[Kind])
                (k : Kargs[Kind] => Unit) {

    map.getOrElse(node, Some(node)) match {
      case Some(n) => k(n, map, nodesToMap)

      case None =>
        nodesToMap match {
          case List() => throw new NoSolution()
          case l =>
            val (sameKind, others) = l partition {n => n.kind == node.kind}
            val choices = new AssumedChoices(k, node, nodesToMap,
              mutable.Set[AGNode[Kind]]() ++ sameKind,
              mutable.Set[AGNode[Kind]]() ++ others)

            newCurrentState(map, choices)
        }
    }
  }

  def attribNode(nodes : List[AGNode[Kind]],
                 map : ResMapping[Kind],
                 nodesToMap : NodesToMap[Kind])
                (k : (List[AGNode[Kind]], ResMapping[Kind], NodesToMap[Kind]) => Unit) {


    def aux(nodes0 : List[AGNode[Kind]], nodes1: List[AGNode[Kind]])(kargs : Kargs[Kind]){
      kargs match {
        case (node, map0, nodesToMap0) =>
          nodes0 match {
            case List() =>
              val l = (node :: nodes1).reverse.tail // we do not forget the last node and we drop the first dummy value
              k(l, map0, nodesToMap0)
            case _ =>
              attribNode(nodes0.head, map0, nodesToMap0){aux(nodes0.tail, node :: nodes1)}
          }
      }
    }
    aux(nodes, List[AGNode[Kind]]())((null, map, nodesToMap)) //null will be dropped in aux
  }



  def removeFirst(l : List[Transformation[Kind]],
                  op : Operation,
                  tgt : TransformationTarget[Kind]) :  Option[List[Transformation[Kind]]] = {

    RecordingComparator.removeFirst(l, {(t : Transformation[Kind]) => t.operation == op && t.target == tgt})
  }


  def printAssociatedChoices(map : ResMapping[Kind]) = {
    map.foreach{
      case (n, Some(mapping)) => println("*  %s ==> %s".format(n, mapping))
      case _ => ()
    }
  }

  def compare(ts1 : List[Transformation[Kind]],
              ts2 : List[Transformation[Kind]],
              map : ResMapping[Kind], nodesToMap : NodesToMap[Kind]){
    if (ts1.isEmpty && ts2.isEmpty) finalStates += currentState
    else {
      def removeFirstAndCompareNext(tgt : TransformationTarget[Kind],
                                    map : ResMapping[Kind], nodesToMap : NodesToMap[Kind]){
        removeFirst(ts2, ts1.head.operation, tgt) match {
          case None => ()
           /* println("Failure on mapping : ")
            println(map.mkString("\t", "\n\t", "\n"))
            println(ts1.head + " mapped as ")
            println(Transformation(ts1.head.operation, tgt) + "cannot be found in :")
            println(ts2.mkString("\t", "\n\t", "\n"))
            println("**************************************")
*/
          case Some(newTs2) =>
            //println("success")
            compare(ts1.tail, newTs2, map, nodesToMap)
        }
      }

      ts1.head.target match {
        case TTEdge(e) => attribNode(List(e.source, e.target), map, nodesToMap) {
          case (List(src, tgt), map1, nodesToMap1) =>
            removeFirstAndCompareNext(TTEdge(AGEdge(e.kind, src, tgt)), map1, nodesToMap1 )
        }

        case TTRedirection(e, extremity) =>
          attribNode(List(e.source, e.target, extremity.node), map, nodesToMap){
            case (List(src, tgt, newExtyNode), map1, nodesToMap1) =>
              removeFirstAndCompareNext(TTRedirection(
                AGEdge(e.kind, src, tgt), extremity.create(newExtyNode)), map1, nodesToMap1)
          }

        /*case TTDependency(dominant, dominated) =>
          attribNode(List(dominant.source, dominant.target,
            dominated.source, dominated.target), map, nodesToMap){
            case (List(dominantSrc, dominantTgt,
            dominatedSrc, dominatedTgt), map1, nodesToMap1) =>
              removeFirstAndCompareNext(TTDependency(AGEdge(dominant.kind, dominantSrc, dominantTgt),
                AGEdge(dominated.kind, dominatedSrc, dominatedTgt)),
                map1, nodesToMap1)
          }


        case TTAbstraction(impl, abs, policy) =>
          attribNode(List(impl, abs), map, nodesToMap){
            case (List(otherImpl, otherAbs), map1, nodesToMap1) =>
              removeFirstAndCompareNext(TTAbstraction(otherImpl, otherAbs, policy), map1, nodesToMap1)
          }
*/
        case TTConstraint(ct, fr) =>
          //TODO give proper definition
          //this should be enough for the tests
          attribNode(fr, map, nodesToMap) {
            case(fr1, map1, nodesToMap1) =>
              removeFirstAndCompareNext(TTConstraint(ct, fr1), map1, nodesToMap1)
          }

        //case TTNode(_) => throw new Error("should not happen !!")
        //removing the dependendency and abstraction of the comparison
        // they are used to compute the change on the graph, its the change themselves we want to compare
        // removed in NodeMappingInitialState.normalizeNodeTransfos
        case TTTypeRedirection(_, _, _) // TODO see if need to be compared
             | TTDependency(_, _)
             | TTAbstraction(_,_,_)
             | TTNode(_) => throw new Error("should not happen !!")

      }
    }
  }

  lazy val initialState = new NodeMappingInitialState(this,
    recording1.composition,
    recording2.composition,
    logger)

  override def search() =
    try {
      super.search()
    } catch {
      case e: NoSolution => None
    }

}
