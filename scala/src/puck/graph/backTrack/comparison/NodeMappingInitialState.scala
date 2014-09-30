package puck.graph.backTrack.comparison

import puck.graph.{AGNode, NodeKind}
import puck.graph.backTrack._

import NodeTransfoStatus.{NodesToMap, ResMapping}
/**
 * Created by lorilan on 30/09/14.
 */
class NodeMappingInitialState[Kind <: NodeKind[Kind]](e : RecordingComparator[Kind],
                                                        lr1 : List[Recordable[Kind]],
                                                        lr2 : List[Recordable[Kind]])
  extends NodeMappingState(0, e, null, null, None) {

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
