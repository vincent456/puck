package puck

import java.io.File

import puck.graph.transformations.Recording
import puck.javaGraph._
import puck.util.PuckNoopLogger

object LoadAndApply {

  def main (args: Array[String]) : Unit = {

    val recFileName = args.head
    val recFile = new File(recFileName)

    val fh = JavaFilesHandler()
    implicit val logger = PuckNoopLogger

    val dg2ast = fh.loadGraph(JGraphUtils.dG2ASTBuilder, null)
    val r = Recording.load(recFile.getAbsolutePath, dg2ast.nodesByName  )
    val g = r.redo(dg2ast.initialGraph)

    dg2ast(g)
    dg2ast.printCode(fh.outDirectory.get)
  }

}
