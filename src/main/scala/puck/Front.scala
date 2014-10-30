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


import puck.javaAG.mutable.JavaFilesHandler

import scala.swing._
 import javax.swing.UIManager
 import puck.gui.PuckMainPanel

 object Front extends SwingApplication{

   def startup(args: Array[String]){
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    if (top.size == new Dimension(0,0)) top.pack()
    top.visible = true
  }

  def top = new MainFrame {
    title = "Puck"

    contents  = new PuckMainPanel(new JavaFilesHandler())

  }
}

/*import java.io.{PipedInputStream, PipedOutputStream}

import org.apache.batik.swing.{JSVGScrollPane, JSVGCanvas}
import puck.graph.Svg
import puck.javaAG.mutable.JavaFilesHandler
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.swing._
import javax.swing.UIManager
import puck.util.FileHelper.string2file

object Front extends SwingApplication{
  val folder = "/home/lorilan/projects/constraintsSolver/scala/test/examples/"
  //val example = "SuperTypeExtraction/01/"
  val example = "Basic/01/"

  val fh = new JavaFilesHandler(folder + example)
  //fh.decouple = "/home/lorilan/puck_svn/distrib/examples/composite/candidate/decouple_strict.pl"
  println("load graph")
  fh.loadGraph(null)
  println("parse cts")
  fh.parseConstraints()
  println("plop")

  override def startup(args: Array[String]){
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    if (top.size == new Dimension(0,0)) top.pack()
    top.visible = true
  }

  def top = new MainFrame {

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)


    import org.apache.batik.dom.svg.SAXSVGDocumentFactory
    import org.apache.batik.util.XMLResourceDescriptor
    val parser = XMLResourceDescriptor.getXMLParserClassName
    val f = new SAXSVGDocumentFactory(parser)


    val canvas = new JSVGCanvas()
    val futDoc = Future {
      f.createSVGDocument(null, pipedInput)
    }

    if(fh.makePng(soutput = Some(pipedOutput), outputFormat = Svg()) ==0)
      println("success")
    else
      println("fail")

    futDoc onSuccess {
      case doc =>  canvas.setSVGDocument(doc)
    }


    title = "Puck"

    //size = new Dimension(401, 220)

    contents  = Component.wrap(new JSVGScrollPane(canvas))
    //contents  = Component.wrap(canvas)


  }
}*/


/*import java.io._

import puck.javaAG.{JavaDefaultDecisionMaker, JavaFilesHandler}


object Front{

  def main(args : Array[String]){

   /* import scala.reflect.runtime.universe._

    val tpe = typeOf[puck.graph.mutable.constraints.search.TryAllCSSE[_]]
    tpe.baseClasses foreach { s => println(s.fullName) }*/

    val folder = "/home/lorilan/projects/constraintsSolver/scala/test/distrib/"
    val example ="prototype/actors/"
   // val example = "bridge/hannemann_inspired/candidate"

/*    val folder = "/home/lorilan/projects/constraintsSolver/scala/test/examples/"
    //val example = "SuperTypeExtraction/01/"
    val example = "Basic/01/"*/

    val fh = new JavaFilesHandler( new File(folder + example))
    //fh.decouple = "/home/lorilan/puck_svn/distrib/examples/composite/candidate/decouple_strict.pl"
    fh.loadGraph(null)
    fh.parseConstraints()
    println("graph loaded")

    print("make png ... ")
    fh.makePng(sOutput = Some(new FileOutputStream(fh.graphFile( "_before.png"))))
    println("done")

    //fh.explore(trace = true)
    fh.graph.transformations.startRegister()

    fh.solve(trace = true, new JavaDefaultDecisionMaker(fh.graph))

    fh.graph.doMerges()

    print("make png ... ")
    fh.makePng(printSignatures = true,
      sOutput = Some(new FileOutputStream(fh.graphFile( "_before.png"))))
    println("done")

    fh.graph.applyChangeOnProgram()

    fh.printCode()


    //println(fh.graph.program)
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
}*/
