package puck


import scala.swing._
import javax.swing.UIManager
import puck.gui.PuckMainPanel


/**
 * Created by lorilan on 08/05/14.
 */

object Front extends SwingApplication{

  /*
  from http://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli
  val usage = """
    Usage: mmlaln [--min-size num] [--max-size num] filename
              """
  def main(args: Array[String]) {
    if (args.length == 0) println(usage)
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "--max-size" :: value :: tail =>
          nextOption(map ++ Map('maxsize -> value.toInt), tail)
        case "--min-size" :: value :: tail =>
          nextOption(map ++ Map('minsize -> value.toInt), tail)
        case string :: opt2 :: tail if isSwitch(opt2) =>
          nextOption(map ++ Map('infile -> string), list.tail)
        case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
        case option :: tail => println("Unknown option "+option)
          exit(1)
      }
    }
    val options = nextOption(Map(),arglist)
    println(options)
  }
  */

  override def startup(args: Array[String]){
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    if (top.size == new Dimension(0,0)) top.pack()
    top.visible = true
  }

  def top = new MainFrame {
    title = "Puck"

    size = new Dimension(300, 200)

    contents  = new PuckMainPanel(FilesHandler())

  }
}


/*

import java.io._
import puck.graph.AccessGraph
import puck.graph.java.JavaSolver


object Front{

  def main(args : Array[String]){
    val fh = FilesHandler("/home/lorilan/puck_svn/distrib/examples/composite/candidate")()
    val graph = fh.loadGraph(null)
    println("graph loaded")
    //fh.accessGraph.list()
    //println(fh.parseConstraints() == fh.parseConstraints())
    graph(fh.parseConstraints())

    println("make png ...")
    fh.makePng(soutput = Some(new FileOutputStream(
      new File(fh.graph.getCanonicalPath + "_before.png"))))

    val nbefore = fh.accessGraph.iterator.toSet
    fh.solve(trace = true)
    val nafter = fh.accessGraph.iterator.toSet

    /*println("Added nodes : ")
    val dif = nafter -- nbefore
    println(dif.mkString("\n"))

    val b = fh.accessGraph.apply("bridge1")
    println("Bridge1 content :")
    println(b.content.mkString("\n"))*/

    println("make png ...")
    fh.makePng(soutput = Some(new FileOutputStream(
      new File(fh.graph.getCanonicalPath + "_after.png"))))


  }
}*/
