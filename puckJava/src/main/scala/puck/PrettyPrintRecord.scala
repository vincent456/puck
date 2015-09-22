package puck

import java.io.File

import puck.graph.transformations.Recording

object PrettyPrintRecord {
  def main (args: Array[String]) : Unit = {

    val recFileName = args.head
    val recFile = new File(recFileName)
    val (_,_,r) = Recording.read(recFile.getAbsolutePath)

    r.reverseIterator foreach println

  }
}
