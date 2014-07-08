package puck.graph.backTrack

import puck.graph.{AGEdge, AGNode}
import puck.search.{SearchEngine, SearchState, FindFirstSearchEngine}

import scala.collection.mutable

/**
 * Created by lorilan on 07/07/14.
 */

object RecordingComparator{
  type NodeTransfoStatus = Int
  val created : NodeTransfoStatus = 1
  val deleted : NodeTransfoStatus = -1
  val neuter : NodeTransfoStatus = 0

  type ResMapping = Map[AGNode, (NodeTransfoStatus, Option[AGNode])]
  type NodesToMap = Map[NodeTransfoStatus, List[AGNode]]

  type Kargs = (AGNode, ResMapping, NodesToMap)

}

import RecordingComparator.{NodeTransfoStatus,ResMapping, NodesToMap, Kargs}
class NoSolution extends Throwable


class AssumedChoices(val node : AGNode,
                     val nts : NodeTransfoStatus,
                     val map : ResMapping,
                     val nodesToMap : NodesToMap,
                     val remainingChoices : mutable.Set[AGNode],
                     val triedChoices : mutable.Set[AGNode])

class NodeMappingState(val id : Int,
                       val engine : SearchEngine[AssumedChoices, Kargs],
                       val internal: AssumedChoices,
                       val k: Kargs => Unit,
                       val prevState : Option[SearchState[AssumedChoices, Kargs]])
  extends SearchState[AssumedChoices, Kargs] {

  def createState(id : Int,
                  engine : SearchEngine[AssumedChoices, Kargs],
                  k: Kargs => Unit,
                  prevState : Option[SearchState[AssumedChoices, Kargs]],
                  choices : AssumedChoices) : NodeMappingState = {
    new NodeMappingState(id, engine, choices, k, prevState)
  }

  def triedAll = internal.remainingChoices.isEmpty

  def executeNextChoice() {
    import internal._
    if(remainingChoices.nonEmpty) {
     if (engine.currentState != this)
        setAsCurrentState()

      val c = remainingChoices.head
      remainingChoices.remove(c)

      val remainingNodesToMap =
        nodesToMap + (nts -> (triedChoices.toList ::: remainingChoices.toList))
      triedChoices.add(c)
      k((c, map + (node ->(nts, Some(c))), remainingNodesToMap))
    }
  }
}


class NodeMappingInitialState (e : RecordingComparator,
                               lr1 : List[Recordable],
                               lr2 : List[Recordable])
  extends NodeMappingState(0, e, null, null, None) {

  def normalizeNodeTransfos(l : List[Recordable]) : (Map[AGNode, Int], List[Transformation])= {

    l.foldLeft( (Map[AGNode, Int](), List[Transformation]()) ){
      case ((m,l1), AddNode(n))=>
        val i = m.getOrElse(n, 0)
        (m + (n -> (i + 1)), l1)
      case ((m,l1), RemoveNode(n)) =>
        val i = m.getOrElse(n, 0)
        (m + (n -> (i - 1)), l1)
      case ((m, l1), t : BreakPoint) => (m , l1)
      case ((m,l1), t : Transformation) => (m, t :: l1)
    }
  }

  val (nodeMap1, remainingTransfos1) = normalizeNodeTransfos(lr1)

  val (nodeMap2, remainingTransfos2) = normalizeNodeTransfos(lr2)

  val initialMapping = nodeMap1.foldLeft(Map[AGNode, (Int, Option[AGNode])]()){
    case (m, (n,i)) => m + (n -> (i, None))
  }

  val otherNodes = nodeMap2.foldLeft(Map[Int, List[AGNode]]()){
    case (m, (n, i)) =>
      val l = m.getOrElse(i, List[AGNode]())
      m + (i -> (n :: l))
  }


  override val triedAll = true

  override def executeNextChoice() {
    e.compare(remainingTransfos1, remainingTransfos2, initialMapping, otherNodes)
  }
}

class RecordingComparator(recording1 : Recording,
                          recording2 : Recording)
  extends FindFirstSearchEngine[AssumedChoices, Kargs] {


  def attribNode(node : AGNode,
                 map : ResMapping,
                 nodesToMap : NodesToMap)
                (k : Kargs => Unit) {

    map.getOrElse(node, (0, Some(node))) match {
      case (_ , Some(n)) => k(n, map, nodesToMap)

      case (i, None) => nodesToMap.get(i) match {
        case None => throw new NoSolution()
        case Some(l) =>
          val choices = new AssumedChoices(node, i, map, nodesToMap,
            mutable.Set[AGNode]() ++ l,
            mutable.Set[AGNode]())

          newCurrentState(k , choices)
      }
    }
  }

  def removeFirst[T](l : List[T], pred : T => Boolean) : Option[List[T]] = {
    def aux(l1 : List[T], acc : List[T]) : Option[List[T]] =
      if(l1.isEmpty) None
      else {
        if(pred(l1.head)) Some(l1.tail ::: acc)
        else aux(l1.tail, l1.head :: acc)
      }

    aux(l, List[T]())
  }
  
  def removeFirst(l : List[Transformation],
             op : Operation,
             tgt : TransformationTarget) :  Option[List[Transformation]] = {

    removeFirst(l, {(t : Transformation) => t.operation == op && t.target == tgt})
  }



  def compare(ts1 : List[Transformation], ts2 : List[Transformation],
              map : ResMapping, nodesToMap : NodesToMap){
    if (ts1. isEmpty) finalStates += currentState
    else {

      def removeFirstAndCompareNext(tgt : TransformationTarget,
                               map : ResMapping, nodesToMap : NodesToMap){
        removeFirst(ts2, ts1.head.operation, tgt) match {
          case None => ()
          case Some(newTs2) => compare(ts1.tail, newTs2, map, nodesToMap)
        }
      }

      ts1.head.target match {
        case TTEdge(e) => attribNode(e.source, map, nodesToMap) {
          case (src, map1, nodesToMap1) =>
            attribNode(e.target, map1, nodesToMap1){
              case (tgt, map2, nodesToMap2) =>
                removeFirst(ts2,
                  ts1.head.operation,
                  TTEdge(AGEdge(e.kind, src, tgt))) match {
                  case Some(newTs2) =>
                    compare(ts1.tail, newTs2, map2, nodesToMap2)
                  case None => ()
                }
            }
        }

        case TTDependency(dominant, dominated) =>
          attribNode(dominant.source, map, nodesToMap){
            case (dominantSrc, map1, nodesToMap1) =>
              attribNode(dominant.target, map1, nodesToMap1){
                case (dominantTgt, map2, nodesToMap2) =>
                  attribNode(dominated.source, map2, nodesToMap2){
                    case (dominatedSrc, map3, nodesToMap3) =>
                      attribNode(dominated.target, map3, nodesToMap3){
                        case (dominatedTgt, map4, nodesToMap4) =>
                          removeFirstAndCompareNext(
                            TTDependency(AGEdge(dominant.kind, dominantSrc, dominantTgt),
                            AGEdge(dominated.kind, dominatedSrc, dominatedTgt)),
                            map4, nodesToMap4)
                      }
                  }
              }
          }

        case TTAbstraction(impl, abs, policy) =>
          attribNode(impl, map, nodesToMap){
            case (otherImpl, map1, nodesToMap1) =>
               attribNode(abs, map1, nodesToMap1){
                 case (otherAbs, map2, nodestoMap2) =>
                   removeFirstAndCompareNext(TTAbstraction(otherImpl, otherAbs, policy),
                       map2, nodestoMap2)

               }

          }

        case TTConstraint(ct) =>
          //TODO give proper definition
          //this should be enough for the tests
          removeFirstAndCompareNext(TTConstraint(ct), map, nodesToMap)

        case TTNode(_) => throw new Error("should not happen !!")
      }
    }
  }

  lazy val initialState = new NodeMappingInitialState(this,
    recording1.composition,
    recording2.composition)

  override def search() = {
    try{
      super.search()
    } catch {
      case e : NoSolution => None
    }

  }
}
