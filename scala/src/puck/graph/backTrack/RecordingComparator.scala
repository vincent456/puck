package puck.graph.backTrack

import puck.graph.{NodeKind, AGEdge, AGNode}
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

  type ResMapping[Kind <: NodeKind[Kind]] = Map[AGNode[Kind], (NodeTransfoStatus, Option[AGNode[Kind]])]
  type NodesToMap[Kind <: NodeKind[Kind]] = Map[NodeTransfoStatus, List[AGNode[Kind]]]

  type Kargs[Kind <: NodeKind[Kind]] = (AGNode[Kind], ResMapping[Kind], NodesToMap[Kind])

}

import RecordingComparator.{NodeTransfoStatus,ResMapping, NodesToMap, Kargs}
class NoSolution extends Throwable


class AssumedChoices[Kind <: NodeKind[Kind]](val node : AGNode[Kind],
                     val nts : NodeTransfoStatus,
                     val map : ResMapping[Kind],
                     val nodesToMap : NodesToMap[Kind],
                     val remainingChoices : mutable.Set[AGNode[Kind]],
                     val triedChoices : mutable.Set[AGNode[Kind]])

class NodeMappingState[Kind <: NodeKind[Kind]](val id : Int,
                       val engine : SearchEngine[AssumedChoices[Kind], Kargs[Kind]],
                       val internal: AssumedChoices[Kind],
                       val k: Kargs[Kind] => Unit,
                       val prevState : Option[SearchState[AssumedChoices[Kind], Kargs[Kind]]])
  extends SearchState[AssumedChoices[Kind], Kargs[Kind]] {

  def createState(id : Int,
                  engine : SearchEngine[AssumedChoices[Kind], Kargs[Kind]],
                  k: Kargs[Kind] => Unit,
                  prevState : Option[SearchState[AssumedChoices[Kind], Kargs[Kind]]],
                  choices : AssumedChoices[Kind]) : NodeMappingState[Kind] = {
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


class NodeMappingInitialState[Kind <: NodeKind[Kind]] (e : RecordingComparator[Kind],
                               lr1 : List[Recordable[Kind]],
                               lr2 : List[Recordable[Kind]])
  extends NodeMappingState(0, e, null, null, None) {

  def normalizeNodeTransfos(l : List[Recordable[Kind]]) : (Map[AGNode[Kind], Int], List[Transformation[Kind]])= {

    l.foldLeft( (Map[AGNode[Kind], Int](), List[Transformation[Kind]]()) ){
      case ((m,l1), Transformation(Add(), TTNode(n)))=>
        val i = m.getOrElse(n, 0)
        (m + (n -> (i + 1)), l1)
      case ((m,l1), Transformation(Remove(), TTNode(n))) =>
        val i = m.getOrElse(n, 0)
        (m + (n -> (i - 1)), l1)
      case ((m, l1), t : BreakPoint[Kind]) => (m , l1)
      case ((m,l1), t : Transformation[Kind]) => (m, t :: l1)
    }
  }

  val (nodeMap1, remainingTransfos1) = normalizeNodeTransfos(lr1)

  val (nodeMap2, remainingTransfos2) = normalizeNodeTransfos(lr2)

  val initialMapping = nodeMap1.foldLeft(Map[AGNode[Kind], (Int, Option[AGNode[Kind]])]()){
    case (m, (n,i)) => m + (n -> (i, None))
  }

  val otherNodes = nodeMap2.foldLeft(Map[Int, List[AGNode[Kind]]]()){
    case (m, (n, i)) =>
      val l = m.getOrElse(i, List[AGNode[Kind]]())
      m + (i -> (n :: l))
  }


  override val triedAll = true

  override def executeNextChoice() {
    e.compare(remainingTransfos1, remainingTransfos2, initialMapping, otherNodes)
  }
}

class RecordingComparator[Kind <: NodeKind[Kind]](recording1 : Recording[Kind],
                          recording2 : Recording[Kind])
  extends FindFirstSearchEngine[AssumedChoices[Kind], Kargs[Kind]] {


  def attribNode(node : AGNode[Kind],
                 map : ResMapping[Kind],
                 nodesToMap : NodesToMap[Kind])
                (k : Kargs[Kind] => Unit) {

    map.getOrElse(node, (0, Some(node))) match {
      case (_ , Some(n)) => k(n, map, nodesToMap)

      case (i, None) => nodesToMap.get(i) match {
        case None => throw new NoSolution()
        case Some(l) =>
          val choices = new AssumedChoices(node, i, map, nodesToMap,
            mutable.Set[AGNode[Kind]]() ++ l,
            mutable.Set[AGNode[Kind]]())

          newCurrentState(k , choices)
      }
    }
  }

  def attribNode(nodes : List[AGNode[Kind]],
                 map : ResMapping[Kind],
                 nodesToMap : NodesToMap[Kind])
                (k : (AGNode[Kind], ResMapping[Kind], NodesToMap[Kind]) => Unit) {

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
  
  def removeFirst(l : List[Transformation[Kind]],
             op : Operation,
             tgt : TransformationTarget[Kind]) :  Option[List[Transformation[Kind]]] = {

    removeFirst(l, {(t : Transformation[Kind]) => t.operation == op && t.target == tgt})
  }



  def compare(ts1 : List[Transformation[Kind]], ts2 : List[Transformation[Kind]],
              map : ResMapping[Kind], nodesToMap : NodesToMap[Kind]){
    if (ts1. isEmpty) finalStates += currentState
    else {

      def removeFirstAndCompareNext(tgt : TransformationTarget[Kind],
                               map : ResMapping[Kind], nodesToMap : NodesToMap[Kind]){
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

        case TTRedirection(e, extremity) => attribNode(e.source, map, nodesToMap){
          case (src, map1, nodesToMap1) =>
            attribNode(e.target, map1, nodesToMap1){
              case(tgt, map2, nodesToMap2) =>
                attribNode(extremity.node, map2, nodesToMap2){
                  case (newExtyNode, map3, nodesToMap3) =>
                    removeFirstAndCompareNext(TTRedirection(AGEdge(src,tgt),
                      extremity.create(newExtyNode)),
                      map3, nodesToMap3)
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

        case TTConstraint(ct, fr) =>
          //TODO give proper definition
          //this should be enough for the tests
        attribNode(fr, map, nodesToMap) {
            case(fr1, map1, nodesToMap1) =>
            removeFirstAndCompareNext(TTConstraint(ct, fr), map, nodesToMap)
          }

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
