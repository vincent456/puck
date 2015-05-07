package puck

import java.io.File

import puck.graph.transformations.Recording
import puck.javaGraph._

object LoadAndApply {

  def main (args: Array[String]) : Unit = {

    val recFileName = args.head
    val recFile = new File(recFileName)

    val fh = JavaFilesHandler()
    fh.loadGraph(null)
    val r = Recording.load(recFile.getAbsolutePath, fh.dg2ast.nodesByName  )
    val g = r.redo(fh.dg2ast.initialGraph)

    fh.applyChangeOnProgram((g,r))
    fh.printCode()
  }

}
