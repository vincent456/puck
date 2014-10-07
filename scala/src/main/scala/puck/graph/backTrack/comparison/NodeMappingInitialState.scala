package puck.graph.backTrack.comparison

import puck.graph._
import puck.graph.backTrack._

import scala.collection.immutable.HashSet

object NodeTransfoStatus {
  def apply( i : Int) = i match {
    case 1 => Created()
    case 0 => Neuter()
    case -1 => Deleted()
    case _ => throw new Error("Illegal node transfo status value !")
  }
}

sealed abstract class NodeTransfoStatus
case class Created() extends NodeTransfoStatus
case class Deleted() extends NodeTransfoStatus
case class Neuter() extends NodeTransfoStatus

class CannotApplySimplificationRule extends Error

/**
 * Created by lorilan on 30/09/14.
 */

object NodeMappingInitialState{

  def normalizeNodeTransfos[Kind <: NodeKind[Kind]](l : List[Recordable[Kind]],
                                                    init : List[Transformation[Kind]] = List()) : (Map[AGNode[Kind], Int], List[Transformation[Kind]])= {
    val (map, rl) = l.foldLeft( (Map[AGNode[Kind], Int](), init) ){
      case ((m,l1), Transformation(Add(), TTNode(n)))=>
        val i = m.getOrElse(n, 0)
        (m + (n -> (i + 1)), l1)
      case ((m,l1), Transformation(Remove(), TTNode(n))) =>
        val i = m.getOrElse(n, 0)
        (m + (n -> (i - 1)), l1)
      case ((m, l1), t : BreakPoint[Kind]) => (m , l1)

      //removing the dependendency and abstraction of the comparison
      // they are used to compute the change on the graph, its the change themselves we want to compare
      //if added back think to uncommment comparison in RecordingComparator.compare
      case ((m,l1), Transformation(_, TTDependency(_, _))) => (m, l1)
      case ((m,l1), Transformation(_, TTAbstraction(_, _, _))) => (m, l1)
      case ((m,l1), Transformation(_, TTConstraint(_,_))) => (m, l1)
      case ((m,l1), t : Transformation[Kind]) => (m, t :: l1)
    }

    (map, rl.reverse)
  }

  def switchNodes[Kind <: NodeKind[Kind], T](nodeStatusesMap : Map[AGNode[Kind], Int],
                                             init : T)
                                            (add : (T, AGNode[Kind]) => T) : (T, List[AGNode[Kind]], Set[AGNode[Kind]]) = {
    nodeStatusesMap.foldLeft[(T, List[AGNode[Kind]], Set[AGNode[Kind]])](init, List(), HashSet()) {
      case ((addAcc, rmAcc, neuterAcc), (node, i)) =>
        NodeTransfoStatus(i) match {
          case Created() => (add(addAcc, node), rmAcc, neuterAcc)
          case Deleted() => (addAcc, node :: rmAcc, neuterAcc)
          case Neuter() => (addAcc, rmAcc, neuterAcc + node)
        }
    }
  }

  def select[T](l : List[T], pred : T => Boolean) : (Option[T], List[T]) = {
    def aux(l1 : List[T], acc : List[T]) : (Option[T], List[T]) =
      if(l1.isEmpty) (None, acc.reverse)
      else if(pred(l1.head)) (Some(l1.head), acc reverse_::: l1.tail)
      else aux(l1.tail, l1.head :: acc)

    aux(l, List[T]())
  }

  def printRule(name : String, op1 : String, op2 : String, res : String ){
    println(name +" : ")
    println(op1)
    println("+ " + op2)
    println("= " + res)
    println("----------------------------------------------")

  }

  def filterNoise[Kind <: NodeKind[Kind]](transfos : List[Transformation[Kind]]):
  List[Transformation[Kind]] = {
    def aux(l1 : List[Transformation[Kind]],
            l2 : List[Transformation[Kind]]): List[Transformation[Kind]] = {
      l2 match {
        case List() => l1.reverse
        case Transformation(_, TTRedirection(_, _)) :: _ =>
          l1.reverse foreach println
          println(" --> " + l2.head)
          l2.tail foreach println
          sys.error(l2.head  + "should be eaten !")
        case (op1 @ Transformation(Add(), TTEdge(AGEdge(Contains(), n1, n2)))) :: tl =>
          select[Transformation[Kind]](tl,
          {
            //case Transformation(Add(), TTEdge(AGEdge(Contains(), `n1`, `n2`))) => true
            case Transformation(Remove(), TTEdge(AGEdge(Contains(), `n1`, `n2`))) => true
            case Transformation(Add(), TTRedirection(AGEdge(Contains(), `n1`, `n2`), Source(_))) => true
            case Transformation(Add(), TTRedirection(AGEdge(Contains(), `n1`, `n2`), Target(_))) =>
            sys.error("contains redirection target should not happen !")
            case _ => false
          }) match {
            case (Some( op2 @ Transformation(Remove(),_)), newTl) =>
              printRule("AddDel", op1.toString, op2.toString, "None")
              aux(l1, newTl)
            case (Some(op2 @ Transformation(Add(), TTRedirection(_, Source(n3)))), newTl) =>
              val res = Transformation(Add(), TTEdge(AGEdge.contains(n3, n2)))
              printRule("AddRed_Src", op1.toString, op2.toString, res.toString)
              aux(l1, res :: newTl)
            case (None, _) => aux(l2.head :: l1, l2.tail)
            case _ => sys.error("should not happen !")
          }
        case (op1 @ Transformation(Add(), TTEdge(AGEdge(kind, n1, n2)))) :: tl =>
          select[Transformation[Kind]](tl,
          { case Transformation(Add(), TTEdge(AGEdge(`kind`, `n1`, `n2`))) => true
            case Transformation(Add(), TTRedirection(AGEdge(`kind`, `n1`, `n2`), _)) => true
            case _ => false
          }) match {
            case (Some( replicate @ Transformation(Add(), TTEdge(_))), newTl) =>
              printRule("AddAdd", op1.toString, replicate.toString, replicate.toString)
              aux (l1, replicate :: newTl)
            case (Some(op2 @ Transformation(Add(), TTRedirection(_, Source(n3)))), newTl) =>
              val res = Transformation(Add(), TTEdge(kind(n3, n2)))
              printRule("AddRed_Src", op1.toString, op2.toString, res.toString)
              aux(l1, res :: newTl)
            case (Some(op2 @ Transformation(Add(), TTRedirection(_, Target(n3)))), newTl) =>
              val res = Transformation(Add(), TTEdge(kind(n1, n3)))
              printRule("AddRed_Tgt", op1.toString, op2.toString, res.toString)
              aux(l1, res :: newTl)

            case (None, _) => aux(l2.head ::l1, l2.tail)
            case _ => sys.error("should not happen !")
          }
        case hd :: tl => sys.error(hd  + " should not be in list")
      }
    }

    aux(List(), transfos)
  }

  def filterDuplicates[Kind <: NodeKind[Kind]](transfos : List[Transformation[Kind]]): List[Transformation[Kind]] = {
    /*can be needed for cases like
     [... add(a,b) .. red((a,b), tgt(c)) ... add(a,d) .. red((a, d), tgt(c)) ... ]
     */
    def aux(l0 : List[Transformation[Kind]],
            l1 : List[Transformation[Kind]]) : List[Transformation[Kind]]= {
      if(l1.isEmpty) l0.reverse
      else aux(l1.head :: l0, l1.tail filter { _ != l1.head })
    }

    aux(List(), transfos)
  }

  /*def filterNoise[Kind <: NodeKind[Kind]](transfos : List[Transformation[Kind]]):
  List[Transformation[Kind]] = {


    val simplificationRules = new SimplificationRules[Kind]()

    var ruleApplied = false


    def aux(l1 : List[Transformation[Kind]],
            l2 : List[Transformation[Kind]]): List[Transformation[Kind]] = {

      l2 match {
        case List() => l1
        case transfo :: tl =>simplificationRules.otherMemberPred(transfo) match {
          case Some(pred) => select(tl, pred) match {
            case (Some(transfo2), newTl) =>
              ruleApplied = true
              simplificationRules(transfo, transfo2) match {
                case Some(newTransfo) => aux( newTransfo :: l1, newTl)
                case None => aux(l1, newTl)
              }

            case (None, _) => aux(transfo :: l1, tl)
          }
          case None => aux(transfo :: l1, tl)
        }
      }
    }

    def fix_aux(transfos : List[Transformation[Kind]]) : List[Transformation[Kind]] = {
      val res = aux(List(), transfos)
      if(!ruleApplied)
        res
      else{
        ruleApplied = false
        fix_aux(res)
      }

    }

    fix_aux(transfos)

  }*/

}

class NodeMappingInitialState[Kind <: NodeKind[Kind]](eng : RecordingComparator[Kind],
                                                      lr1 : List[Recordable[Kind]],
                                                      lr2 : List[Recordable[Kind]])
  extends NodeMappingState(0, eng, null, null, None) {

  //println("creating initial state")
  //println("Comparing %s and %s".format(recording1, recording2))


  import NodeMappingInitialState._


  val (nodeStatuses, remainingTransfos1) = normalizeNodeTransfos(lr1, eng.initialTransfos)

  val (nodeStatuses2, remainingTransfos2) = normalizeNodeTransfos(lr2, eng.initialTransfos)

  /* def switch(nts : NodeTransfoStatus, c : Int, d : Int, n : Int) = nts match {
     case Created() => (c + 1 , d , n)
     case Deleted() => (c, d + 1, n)
     case Neuter() => (c, d, n + 1)
   }*/

  val (initialMapping, removedNode, neuterNodes) =
    switchNodes(nodeStatuses, Map[AGNode[Kind], Option[AGNode[Kind]]]()){
      case (m, n) => m + (n -> None)
    }

  val (nodesToMap, otherRemovedNodes, otherNeuterNodes) =
    switchNodes(nodeStatuses2, List[AGNode[Kind]]()){case (l, n) => n :: l}



  var triedAll0 = false

  override def triedAll = triedAll0

  def printlnNode(n : AGNode[Kind]) {println("%d = %s(%s)".format(n.id, n.kind, n.fullName))}

  override def executeNextChoice() {
    triedAll0 = true

    if( nodesToMap.length != initialMapping.size ||
      !(removedNode forall otherRemovedNodes.contains)) {
      val sameNumberOfNodesToMap = nodesToMap.length == initialMapping.size
      val sameRemovedNodes = removedNode forall otherRemovedNodes.contains
      println("sameNumberOfNodesToMap  = " + sameNumberOfNodesToMap)
      println("same removed nodes = " + sameRemovedNodes)

      println("initialMapping : %s, %d remaining transfo".format(initialMapping, remainingTransfos1.size))

      println("created nodes :")
      initialMapping.keys foreach printlnNode
      println("neuter nodes : ")
      neuterNodes foreach printlnNode

      println("*******************************************************")
      println()
      println()

      println("nodes to map : %s, %d remaining transfo".format(nodesToMap, remainingTransfos2.size))

      println("created nodes :")
      nodesToMap foreach printlnNode
      println("neuter nodes : ")
      otherNeuterNodes foreach printlnNode

      throw new NoSolution()
    }
    else {




      println("initialMapping : %s, %d remaining transfo".format(initialMapping, remainingTransfos1.size))

      println("created nodes :")
      initialMapping.keys foreach printlnNode
      println("neuter nodes : ")
      neuterNodes foreach printlnNode

      println()
      println()
      remainingTransfos1 foreach println

      println("*******************************************************")
      println("***************** filtering 1 *************************")
      println("*******************************************************")

      val filteredTransfos1 = if(neuterNodes.nonEmpty) filterDuplicates(filterNoise(remainingTransfos1))
      else remainingTransfos1

      println("*******************************************************")
      println("*******************************************************")

      filteredTransfos1 foreach println

      println()
      println()
      println("*******************************************************")
      println("*******************************************************")
      println("************************* and *************************")
      println("*******************************************************")
      println("*******************************************************")
      println()
      println()

      println("nodes to map : %s, %d remaining transfo".format(nodesToMap, remainingTransfos2.size))

      println("created nodes :")
      nodesToMap foreach printlnNode
      println("neuter nodes : ")
      otherNeuterNodes foreach printlnNode

      println()
      println()
      remainingTransfos2.foreach(println)


      println("*******************************************************")
      println("***************** filtering 2 *************************")
      println("*******************************************************")

      val filteredTransfos2 = if(otherNeuterNodes.nonEmpty) filterDuplicates(filterNoise(remainingTransfos2))
      else remainingTransfos2

      println("*******************************************************")
      println("*******************************************************")

      filteredTransfos2 foreach println

      eng.compare(filteredTransfos1, filteredTransfos2, initialMapping, nodesToMap)
      /*eng.compare(filteredTransfos1, filteredTransfos2,
        neuterNodes.foldLeft(initialMapping){ (m, n) => m + (n -> None) },
        nodesToMap ++ otherNeuterNodes)*/
      //eng.compare(remainingTransfos1, remainingTransfos2, initialMapping, nodesToMap)

    }


  }
}



