package puck

import java.io.{File, FileReader}

import puck.graph.constraints.ConstraintsParser
import puck.graph.io.VisibilitySet._
import puck.graph.io._
import puck.graph.{DependencyGraph, NodeId}
import puck.jastadd.CompileHelper
import puck.javaGraph.JavaDotHelper

object Java2dot {

  def quickDot
  ( outFileName : String,
    dg : DependencyGraph,
    fullName2id : Map[String, NodeId]) : Unit = {

    val vis =
      VisibilitySet.allVisible(dg)
        .setVisibility(dg.subTree(fullName2id("java")), Hidden)
        .setVisibility(dg.subTree(fullName2id("@primitive")), Hidden)

    val options = PrintingOptions(vis, printId = true, printSignatures = false, selectedUse = None)
    DotPrinter.genDotFile(dg, JavaDotHelper, options, outFileName)
  }

  def main (args: Array[String]) : Unit = {

    val outFileName = args.head
    val decouple = new File(args.tail.head)
    val srcs = args.tail.tail.toList

    val (_, dg , _, fullName2id, _) =
      CompileHelper.compileSrcsAndbuildGraph(sources = srcs, jars = List(), bootJars = List(), decouple = Some(decouple))

    val cm = ConstraintsParser(fullName2id, new FileReader(decouple))

    quickDot(outFileName, dg.newGraph(constraints = cm), fullName2id)
  }
}
