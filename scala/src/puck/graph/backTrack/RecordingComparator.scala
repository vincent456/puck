package puck.graph.backTrack

import puck.graph.{NodeKind, AGEdge, AGNode}
import puck.search.{StateCreator, SearchEngine, SearchState, FindFirstSearchEngine}

import scala.collection.mutable

/**
 * Created by lorilan on 07/07/14.
 */

object NodeTransfoStatus {
  def apply( i : Int) = i match {
    case 1 => Created()
    case 0 => Neuter()
    case -1 => Deleted()
    case _ => throw new Error("Illegal node transfo status value !")
  }

  type ResMapping[Kind <: NodeKind[Kind]] = Map[AGNode[Kind], (NodeTransfoStatus, Option[AGNode[Kind]])]
  type NodesToMap[Kind <: NodeKind[Kind]] = Map[NodeTransfoStatus, List[AGNode[Kind]]]

  type Kargs[Kind <: NodeKind[Kind]] = (AGNode[Kind], ResMapping[Kind], NodesToMap[Kind])
}
sealed abstract class NodeTransfoStatus
case class Created() extends NodeTransfoStatus
case class Deleted() extends NodeTransfoStatus
case class Neuter() extends NodeTransfoStatus


import NodeTransfoStatus.{ResMapping, NodesToMap, Kargs}

class NoSolution extends Throwable

class AssumedChoices[Kind <: NodeKind[Kind]](val k: Kargs[Kind] => Unit,
                                             val node : AGNode[Kind],
                                             val nts : NodeTransfoStatus,
                                             val map : ResMapping[Kind],
                                             val nodesToMap : NodesToMap[Kind],
                                             val remainingChoices : mutable.Set[AGNode[Kind]],
                                             val triedChoices : mutable.Set[AGNode[Kind]])
  extends StateCreator[AssumedChoices[Kind], AssumedChoices[Kind]] {

  def createState (id: Int,
                   engine: SearchEngine[AssumedChoices[Kind]],
                   prevState: Option[SearchState[_, AssumedChoices[Kind]]],
                   choices: AssumedChoices[Kind] ): NodeMappingState[Kind] = {
    new NodeMappingState (id, engine, choices, prevState)
  }
}




class NodeMappingState[Kind <: NodeKind[Kind]](val id : Int,
                                               val engine : SearchEngine[AssumedChoices[Kind]],
                                               val internal: AssumedChoices[Kind],
                                               val prevState : Option[SearchState[_, AssumedChoices[Kind]]])
  extends SearchState[AssumedChoices[Kind], AssumedChoices[Kind]] {



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
  extends NodeMappingState(0, e, null, None) {

  //println("creating initial state")

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

  def switch(nts : NodeTransfoStatus, c : Int, d : Int, n : Int) = nts match {
    case Created() => (c + 1 , d , n)
    case Deleted() => (c, d + 1, n)
    case Neuter() => (c, d, n + 1)
  }

  val (initialMapping, statuses) =
    nodeMap1.foldLeft[(ResMapping[Kind], (Int, Int, Int))]((Map(), (0,0,0))){
      case ((m, (c, d , n)), (node,i)) =>
        val nts = NodeTransfoStatus(i)
        (m + (node -> (nts, None)), switch(nts, c, d , n))
    }

  val (otherNodes, statuses2) =
    nodeMap2.foldLeft[(NodesToMap[Kind], (Int, Int, Int))](Map(), (0,0,0)){
      case ((m, (c, d, n)), (node, i)) =>
        val nts = NodeTransfoStatus(i)
        val l = m.getOrElse(nts, List[AGNode[Kind]]())
        (m + (nts -> (node :: l)), switch(nts, c, d, n))
    }

  /*
    println("initialMapping : %s, (c, d, n) : %s, %d remaining transfo".format(initialMapping, statuses, remainingTransfos1.size))
    println("nodes to map : %s, (c, d, n) : %s, %d remaining transfo".format(otherNodes, statuses2, remainingTransfos2.size))
  */

  /*println("nodes to map : %s, (c, d, n) : %s, %d remaining transfo".format(otherNodes, statuses2, remainingTransfos2.size))
  println("nodes to map : %s, (c, d, n) : %s, %d remaining transfo".format(otherNodes, statuses2, remainingTransfos2.size))
*/
  var triedAll0 = false

  override def triedAll = triedAll0

  override def executeNextChoice() {
    triedAll0 = true
    if(statuses != statuses2)
      throw new NoSolution()
    else
      e.compare(remainingTransfos1, remainingTransfos2, initialMapping, otherNodes)

  }
}

class RecordingComparator[Kind <: NodeKind[Kind]](recording1 : Recording[Kind],
                                                  recording2 : Recording[Kind])
  extends FindFirstSearchEngine[AssumedChoices[Kind]] {



  def attribNode(node : AGNode[Kind],
                 map : ResMapping[Kind],
                 nodesToMap : NodesToMap[Kind])
                (k : Kargs[Kind] => Unit) {

    map.getOrElse(node, (Neuter(), Some(node))) match {
      case (_ , Some(n)) => k(n, map, nodesToMap)

      case (i, None) => nodesToMap.get(i) match {
        case None => throw new NoSolution()
        case Some(l) =>
          val choices = new AssumedChoices(k, node, i, map, nodesToMap,
            mutable.Set[AGNode[Kind]]() ++ l,
            mutable.Set[AGNode[Kind]]())

          newCurrentState(choices)//TODO check if better solution
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
    //TODO change !!!
    if (ts1.isEmpty && ts2.isEmpty) finalStates += currentState.asInstanceOf[SearchState[AssumedChoices[Kind], AssumedChoices[Kind]]]
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
