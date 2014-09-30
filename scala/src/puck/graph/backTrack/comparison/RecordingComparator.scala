package puck.graph.backTrack.comparison

import puck.graph.backTrack._
import puck.graph.{AGEdge, AGNode, NodeKind}
import puck.search.FindFirstSearchEngine

import scala.collection.mutable

/**
 * Created by lorilan on 07/07/14.
 */

import puck.graph.backTrack.comparison.NodeTransfoStatus.{Kargs, NodesToMap, ResMapping}

class NoSolution extends Throwable

class RecordingComparator[Kind <: NodeKind[Kind]](recording1 : Recording[Kind],
                                                  recording2 : Recording[Kind])
  extends FindFirstSearchEngine[ResMapping[Kind]] {

  def attribNode(node : AGNode[Kind],
                 map : ResMapping[Kind],
                 nodesToMap : NodesToMap[Kind])
                (k : Kargs[Kind] => Unit) {

    map.getOrElse(node, (Neuter(), Some(node))) match {
      case (_ , Some(n)) => k(n, map, nodesToMap)

      case (i, None) => nodesToMap.get(i) match {
        case None => throw new NoSolution()
        case Some(l) =>
          val choices = new AssumedChoices(k, node, i, nodesToMap,
            mutable.Set[AGNode[Kind]]() ++ l,
            mutable.Set[AGNode[Kind]]())

          newCurrentState(map, choices)//TODO check if better solution
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

  def removeFirst[T](l : List[T], pred : T => Boolean) : Option[List[T]] = {
    def aux(l1 : List[T], acc : List[T]) : Option[List[T]] =
      if(l1.isEmpty) None
      else if(pred(l1.head)) Some(l1.tail ::: acc)
      else aux(l1.tail, l1.head :: acc)


    aux(l, List[T]())
  }

  def removeFirst(l : List[Transformation[Kind]],
                  op : Operation,
                  tgt : TransformationTarget[Kind]) :  Option[List[Transformation[Kind]]] = {

    removeFirst(l, {(t : Transformation[Kind]) => t.operation == op && t.target == tgt})
  }


  def printAssociatedChoices(map : ResMapping[Kind]) = {
    map.foreach{
      case (n, (st, Some(mapping))) => println("*  %s ==> %s".format(n, mapping))
      case _ => ()
    }
  }

  def compare(ts1 : List[Transformation[Kind]], ts2 : List[Transformation[Kind]],
              map : ResMapping[Kind], nodesToMap : NodesToMap[Kind]){
    if (ts1.isEmpty && ts2.isEmpty) finalStates += currentState
    else {
      def removeFirstAndCompareNext(tgt : TransformationTarget[Kind],
                                    map : ResMapping[Kind], nodesToMap : NodesToMap[Kind]){
        removeFirst(ts2, ts1.head.operation, tgt) match {
          case None => ()
          case Some(newTs2) => compare(ts1.tail, newTs2, map, nodesToMap)
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

        case TTDependency(dominant, dominated) =>
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

        case TTConstraint(ct, fr) =>
          //TODO give proper definition
          //this should be enough for the tests
          attribNode(fr, map, nodesToMap) {
            case(fr1, map1, nodesToMap1) =>
              removeFirstAndCompareNext(TTConstraint(ct, fr1), map1, nodesToMap1)
          }

        case TTNode(_) => throw new Error("should not happen !!")
      }
    }
  }

  lazy val initialState = new NodeMappingInitialState(this,
    recording1.composition,
    recording2.composition)

  override def search() =

    try{
      super.search()
    } catch {
      case e : NoSolution => None
    }

}
