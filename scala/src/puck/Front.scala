package puck


/**
 * Created by lorilan on 08/05/14.
 */

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



/*
 import scala.swing._
 import javax.swing.UIManager
 import puck.gui.PuckMainPanel

 object Front extends SwingApplication{
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
*/



import java.io._
import puck.graph.DotPrinter
import puck.gui.GUIDecisionMaker
import puck.javaAG.{JavaNode, DefaultDecisionMaker}

object Front{

  def main(args : Array[String]){

    //val example ="prototype/actors/candidate"
    val example = "bridge/hannemann_inspired/candidate"

    val fh = FilesHandler("/home/lorilan/puck_svn/distrib/examples/" +
      example)()
    //fh.decouple = "/home/lorilan/puck_svn/distrib/examples/composite/candidate/decouple_strict.pl"
    fh.loadGraph(null)
    println("graph loaded")


    //fh.accessGraph.list()
    //println(fh.parseConstraints() == fh.parseConstraints())
    fh.parseConstraints()

    print("make png ... ")
    fh.makePng(soutput = Some(new FileOutputStream(
      new File(fh.graphStubFile.getCanonicalPath + "_before.png"))))
    println("done")

    /*fh.accessGraph.transformations.startRegister()
    fh.solve(trace = true,
      decisionMaker = new DefaultDecisionMaker(fh.accessGraph))

    val g = fh.accessGraph.transformations.recording.partialGraph()

    DotPrinter.print(new BufferedWriter(new FileWriter(fh.graph.getCanonicalPath+"_partial.dot")),
      g, JavaNode, printId=false, searchRoots = true)

    g.nodes.foreach(println)

    println(fh.accessGraph.transformations.getClass)

    val r = fh.accessGraph.transformations.recording
    println("undo everything !")
    r.undo()

    print("make png ... ")
    fh.makePng(soutput = Some(new FileOutputStream(
      new File(fh.graph.getCanonicalPath + "_undone.png"))))
    println("done")

    r.redo()

    print("make png ... ")
    fh.makePng(soutput = Some(new FileOutputStream(
      new File(fh.graph.getCanonicalPath + "_redone.png"))))
    println("done")*/

    fh.explore(trace = true)


    //    print("make pl ... ")
    //
    //    scala.Console.withOut(new FileOutputStream(fh.srcDirectory+ "/decouple_after.pl")) {
    //      fh.accessGraph.printConstraints()
    //    }
    //    fh.makeProlog()
    //    println("done")
    //
    //    fh.accessGraph.printConstraints()
  }
}
