package puck

import java.io.File

import puck.graph.transformations.Recording
import puck.javaGraph._

object LoadAndApply {

  def main (args: Array[String]) : Unit = {

    val recFileName = args.head
    val recFile = new File(recFileName)

    val fh = JavaFilesHandler()
    val dg2ast = fh.loadGraph(JGraphUtils.dG2ASTBuilder, null)
    val r = Recording.load(recFile.getAbsolutePath, dg2ast.nodesByName  )
    val g = r.redo(dg2ast.initialGraph)

    dg2ast(g)
    fh.printCode(dg2ast)
  }

}
