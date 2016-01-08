package puck

import java.io.File

import puck.graph.DependencyGraph
import puck.graph.comparison.Mapping
import puck.graph.io.{FilesHandler, DG2AST}
import puck.util.PuckLogger
import sbt.IO

/**
  * Created by lorilan on 05/01/16.
  */
trait FilesHandlerDG2ASTControllerOps {

  implicit val logger : PuckLogger
  def graph : DependencyGraph
  def dg2ast : DG2AST
  val filesHandler : FilesHandler

  def deleteOutDirAndapplyOnCode() : Unit = {
    logger.writeln("Aplying recording on AST")
    dg2ast(graph)/*(new PuckFileLogger(_ => true, new File("/tmp/pucklog")))*/

    filesHandler.outDirectory.get match {
      case None => logger.writeln("no output directory : cannot print code")
      case Some(d) =>
        logger.writeln("Printing code")
        IO.delete(d)
        dg2ast.printCode(d)
    }

  }

  def compareOutputGraph() : Unit = {
    val outfh = filesHandler.fromOutDir
    logger.writeln("Loading output graph from code")
    val outdg2ast = outfh.loadGraph()
    logger.writeln("Comparing graphs ...")

    val res = if(Mapping.equals(graph, outdg2ast.initialGraph)) "EQUAL"
    else "NOT equal"

    logger.writeln(s"they are $res")
  }

  def workingDirectory : File = filesHandler.workingDirectory
}
