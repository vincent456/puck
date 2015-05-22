package puck

import java.io.{FileReader, File, FileWriter}

import puck.graph.{NodeId, DependencyGraph}
import puck.graph.constraints.ConstraintsParser
import puck.graph.io.{Hidden, VisibilitySet, PrintingOptions, FilesHandler}
import puck.javaGraph.{JavaDotHelper, CompileHelper}

object Java2dot {

  def quickDot
  ( outFileName : String,
    dg : DependencyGraph,
    fullName2id : Map[String, NodeId]) : Unit = {
    val fos = new FileWriter(outFileName)

    val vis = VisibilitySet.allVisible(dg)
    vis.setVisibility(dg.subTree(fullName2id("java")), Hidden)
    vis.setVisibility(dg.subTree(fullName2id("@primitive")), Hidden)

    val options = PrintingOptions(vis, printId = true, printSignatures = false, selectedUse = None)
    FilesHandler.makeDot(dg, JavaDotHelper, options, fos)
    fos.close()
  }

  def main (args: Array[String]) : Unit = {

    val outFileName = args.head
    val decouple = new File(args.tail.head)
    val srcs = args.tail.tail.toList

    val (_, dg , _, fullName2id, _) =
      CompileHelper.compileSrcsAndbuildGraph(sources = srcs, jars = List(), decouple = Some(decouple))

    val cm = ConstraintsParser(fullName2id, new FileReader(decouple))

    quickDot(outFileName, dg.newGraph(constraints = cm), fullName2id)
  }
}
