package puck

import java.io.File
import puck.graph.ShowDG
import puck.graph.transformations.Recording
import puck.util.PuckSystemLogger
import jastadd._
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

    val fh = JavaProject()
    implicit val logger = new PuckSystemLogger(_ => true)

    val dg2ast = fh.loadGraph()

    val r = Recording.load(recFile.getAbsolutePath, dg2ast.nodesByName)

    import ShowDG._
    val _ = r.reverse.foldLeft(dg2ast.initialGraph){(g0, t) =>
      (g0, t).println
      t.redo(g0)
    }

  }

}