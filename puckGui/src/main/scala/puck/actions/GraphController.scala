package puck.actions

import puck.GraphStack
import puck.graph._

/**
  * Created by lorilan on 15/12/15.
  */
trait GraphController {
  val graphUtils : GraphUtils
  val graphStack : GraphStack

  def graph = graphStack.graph
  implicit def logger = graphStack.logger
  def selectedNodes: List[NodeId]
  def selectedEdge : Option[NodeIdP]

}
