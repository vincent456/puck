package puck.javaGraph

import java.io.File

import puck.graph.transformations.Transformation
import puck.graph.{DependencyGraph, NodeId}
import puck.jastadd.CompileHelper
import puck.util.{FileHelper, PuckFileLogger, PuckLogger}
import sbt.IO
import org.extendj.ast.{ASTNodeLink, JavaJastAddDG2AST, Program}

object ScenarioFactory {
  def fromDirectory(path: String): ScenarioFactory = {
     new ScenarioFactory(FileHelper.findAllFiles(new File(path), ".java", Some("out")):_*)
  }
}


case class ScenarioFactory
( program : Program,
  graph : DependencyGraph,
  initialRecord : Seq[Transformation],
  fullName2id : Map[String, NodeId],
  dg2astMap : Map[NodeId, ASTNodeLink]){

  def this(t : (Program,
                DependencyGraph,
                Seq[Transformation],
                Map[String, NodeId],
                Map[NodeId, ASTNodeLink])) =
        this(t._1, t._2, t._3, t._4, t._5)

  def this(filePath : String) =
    this(CompileHelper.compileSrcsAndbuildGraph(List(filePath), List(), List(), List()))
  def this(filesPath : String*) =
    this(CompileHelper.compileSrcsAndbuildGraph(filesPath.toList, List(), List(), List()))

  implicit var logger : PuckLogger = new PuckFileLogger(_ => true, new java.io.File("/tmp/comparisonLog"))
  def compare: (DependencyGraph, DependencyGraph) => Boolean =
    (g1, g2) => DependencyGraph.areEquivalent(initialRecord,g1,g2, logger)


  def applyChanges(g: DependencyGraph,
                   outDir : File) : Unit = {
    val dg2ast = new JavaJastAddDG2AST(program, graph, initialRecord, fullName2id, dg2astMap)

    dg2ast.apply(g)(new PuckFileLogger(_ => true, new File("/tmp/pucklog")))
    IO.delete(outDir)
    dg2ast.printCode(outDir)
  }
  def applyChangeAndMakeExample
  ( g: DependencyGraph,
    outDir : File) : ScenarioFactory = {
    applyChanges(g, outDir)

    val genSrc = FileHelper.findAllFiles(outDir, ".java", None)
    new ScenarioFactory(genSrc:_*)
  }

  def printFullNames() : Unit =
    fullName2id.keys.toList.sorted foreach println

  def printFullNamesSortedByKey() : Unit =
    fullName2id.toList map (_.swap) sortBy(_._1) foreach println
}
