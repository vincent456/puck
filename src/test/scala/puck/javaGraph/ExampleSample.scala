package puck.javaGraph

import puck.graph.{DependencyGraph, NodeId}
import puck.graph.transformations.Recording
import puck.util.{PuckNoopLogger, PuckLogger}

case class ExampleSample
( program : AST.Program,
  graph : DependencyGraph,
  initialRecord : Recording,
  fullName2id : Map[String, NodeId],
  dg2astMap : Map[NodeId, ASTNodeLink]){

  def this(t : (AST.Program,
                DependencyGraph,
                Recording,
                Map[String, NodeId],
                Map[NodeId, ASTNodeLink])) =
        this(t._1, t._2, t._3, t._4, t._5)

  def this(filePath : String) =
    this(CompileHelper.compileSrcsAndbuildGraph(List(filePath), List()))
  def this(filesPath : String*) =
    this(CompileHelper.compileSrcsAndbuildGraph(filesPath.toList, List()))

  var logger : PuckLogger = PuckNoopLogger
  def compare: (DependencyGraph, DependencyGraph) => Boolean =
    (g1, g2) => DependencyGraph.areEquivalent(initialRecord,g1,g2, logger)
}
