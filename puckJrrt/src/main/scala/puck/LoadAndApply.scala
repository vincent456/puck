package puck

import java.io.File

import puck.graph.comparison.Mapping
import puck.graph.transformations.Recording
import puck.util.PuckSystemLogger
import puck.jastadd.JavaFilesHandler
import sbt.IO

object LoadAndApply {

  def main (args: Array[String]) : Unit = {

    /*val recFileName = args.head
    val recFile = new File(recFileName)*/
    val recFile = new File(FrontVars.workspace + "/planB2.puck")
    val fh = JavaFilesHandler(FrontVars.workspace)
    implicit val logger = new PuckSystemLogger(_ => true)

    val dg2ast = fh.loadGraph()

    val r = Recording.load(recFile.getAbsolutePath, dg2ast.nodesByName  )
    val g = r.redo(dg2ast.initialGraph)

    val outDirectory = fh.outDirectory !

    if(outDirectory.exists())
      IO.delete(outDirectory)


    dg2ast(g)
    dg2ast.printCode(outDirectory)


    val fhout = JavaFilesHandler(outDirectory)
    val dg2astout = fhout.loadGraph()

    val gout = dg2astout.initialGraph

    try {
      if(Mapping.equals(g, gout)){
        println("ARE equals")
      }
      else {
        println("are NOT equals")
        import puck.graph.ShowDG.DGShowOp
        import puck.util.Debug.nodeIndexCordBuilder
//        import scalaz.syntax.show._
//        import puck.util.Debug.showNodeIndex
        println((g, g.nodesIndex).shows)
        println((gout, gout.nodesIndex).shows)

//        val mapping = Mapping.create(g, gout)
//
//        println(Mapping.mapCVM(mapping, g.edges.userMap))
//        println(gout.edges.userMap)
      }
    } catch {
      case e : PuckError => println(e.getMessage)
    }
  }

}
