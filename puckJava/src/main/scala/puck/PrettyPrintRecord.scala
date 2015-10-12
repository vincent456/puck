package puck

import java.io.File
import javaGraph.JavaFilesHandler
import puck.graph.ShowDG
import puck.graph.transformations.Recording
import puck.javaGraph.JGraphUtils
import puck.util.PuckSystemLogger

//object PrettyPrintRecord {
//  def main (args: Array[String]) : Unit = {
//
//    val recFileName = args.head
//    val recFile = new File(recFileName)
//    val (_,_,r) = Recording.read(recFile.getAbsolutePath)
//
//    r.reverseIterator foreach println
//
//  }
//}

object PrettyPrintRecord {

  def main (args: Array[String]) : Unit = {

    val recFileName = args.head
    val recFile = new File(recFileName)

    val fh = JavaFilesHandler()
    implicit val logger = new PuckSystemLogger(_ => true)

    val dg2ast = fh.loadGraph(JGraphUtils.dG2ASTBuilder, null)

    val r = Recording.load(recFile.getAbsolutePath, dg2ast.nodesByName)

    import ShowDG._
    r.reverse.foldLeft(dg2ast.initialGraph){(g0, t) =>
      (g0, t).println
      t.redo(g0)
    }

  }

}