package puck.javaGraph

import puck.graph.DependencyGraph
import puck.graph.DependencyGraph.NodeId
import puck.graph.transformations.Recording
import puck.util.{PuckNoopLogger, PuckLogger}

/**
 * Created by lorilan on 2/25/15.
 */
case class ExampleSample
( program : AST.Program,
  graph : DependencyGraph,
  initialRecord : Recording,
  fullName2id : Map[String, NodeId],
  dg2ast : Map[NodeId, ASTNodeLink]){

  def this(t : (AST.Program,
                DependencyGraph,
                Recording,
                Map[String, NodeId],
                Map[NodeId, ASTNodeLink])) =
        this(t._1, t._2, t._3, t._4, t._5)

  def this(filePath : String) =
    this(CompileHelper.buildGraph(List(filePath), List()))

  var logger : PuckLogger = PuckNoopLogger
  def compare: (DependencyGraph, DependencyGraph) => Boolean =
    (g1, g2) => DependencyGraph.areEquivalent(initialRecord,g1,g2, logger)
}
