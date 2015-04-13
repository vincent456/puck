package puck

import java.io.FileWriter

import puck.graph.io.{Hidden, VisibilitySet, PrintingOptions, FilesHandler}
import puck.javaGraph.{JavaDotHelper, CompileHelper}

object Java2dot {
  def main (args: Array[String]) : Unit = {
    val (_, dg , _, fullName2id, _) =
      CompileHelper.compileSrcsAndbuildGraph(sources = args.tail.toList, jars = List(), decouple = None)

    val fos = new FileWriter(args.head)

    val vis = VisibilitySet.allVisible(dg)
    vis.setVisibility(dg.subTree(fullName2id("java")), Hidden)
    vis.setVisibility(dg.subTree(fullName2id("@primitive")), Hidden)

    val options = PrintingOptions(vis, printId = false, printSignatures = false, selectedUse = None)
    FilesHandler.makeDot(dg, JavaDotHelper, options, fos)
  }
}
