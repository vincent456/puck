package puck.gui.imageDisplay

import puck.graph.constraints.AbstractionPolicy
import puck.graph.{NodeKind, DGNode, NodeId, DependencyGraph}

import scala.collection.mutable
import scala.collection.JavaConversions._
/**
 * Created by lorilan on 3/16/15.
 */

class SVGController {

  val graphStack = mutable.Stack[DependencyGraph]()

  var nodeSelected : Option[NodeId] = None

  def setNodeSelected(id : NodeId) = Some(id)
  def resetNodeSelected = nodeSelected = None


  def pushGraph(graph : DependencyGraph) =
    graphStack.push(graph)

  def popGraph() = graphStack.pop()

  def getGraph = graphStack.head

  case class AbstractionChoice(policy : AbstractionPolicy, kind : NodeKind)
  def abstractionChoices(n : DGNode) : java.util.List[AbstractionChoice] = {
    val l = for{
      p <- n.kind.abstractionPolicies
      k <- n.kind.abstractKinds(p)
    } yield {AbstractionChoice(p,k)}

    seqAsJavaList(l)
  }
  def abstractionChoices(id : NodeId) : java.util.List[AbstractionChoice] = abstractionChoices(getGraph.getNode(id))

}
