package puck

import java.io._

import puck.graph.{AGError, DotPrinter, AGBuildingError, AccessGraph}
import puck.javaAG.{JavaAccessGraph, DefaultDecisionMaker, JavaSolver, JavaNode}
import scala.sys.process.Process
import puck.graph.constraints.{DecisionMaker, ConstraintsParser}
import java.util.NoSuchElementException

/**
 * Created by lorilan on 08/05/14.
 */
class FilesHandler private (private [this] var srcDir : File,
                            private [this] var jarlistFile0: File,
                            private [this] var apiNodesFile0 :File,
                            private [this] var decouple0 : File,
                            private [this] var graph0: File){


  private [this] var ag : JavaAccessGraph = _
  def accessGraph = ag

  def srcDirectory = this.srcDir
  def srcDirectory_=(dir : File){ this.srcDir = dir.getCanonicalFile
    jarlistFile0 = FilesHandler.defaultFile(srcDir, FilesHandler.defaultJarListFileName)
    apiNodesFile0 = FilesHandler.defaultFile(srcDir, FilesHandler.defaultApiNodesFileName)
    decouple0 = FilesHandler.defaultFile(srcDir, FilesHandler.defaultDecoupleFileName)
    graph0 = FilesHandler.defaultFile(srcDir, FilesHandler.defaultGraphFileName)
  }

  def jarListFile = this.jarlistFile0
  def jarListFile_=(f:File){ this.jarlistFile0 = f.getCanonicalFile}

  def apiNodesFile = this.apiNodesFile0
  def apiNodesFile_=(f:File){this.apiNodesFile0= f.getCanonicalFile}

  private [this] var gdot : File = _
  def graphvizDot = this.gdot
  def graphvizDot_=(f: File){this.gdot = f.getCanonicalFile}

  def decouple = this.decouple0
  def decouple_=(f:File){this.decouple0 = f.getCanonicalFile}

  def graph = this.graph0
  def graph_=(f: File){this.graph0= f.getCanonicalFile}

  def loadGraph(ll : AST.LoadingListener) : AccessGraph = {
    FilesHandler.compile(FilesHandler.findAllJavaFiles(this.srcDirectory),
      puck.fileLines(jarListFile)) match {
      case None => throw new AGBuildingError("Compilation error, no AST generated")
      case Some(p) =>
        ag = p.buildAccessGraph(puck.initStringLiteralsMap(decouple), ll)
        fileLines(apiNodesFile).foreach {
          (l: String) =>
            val tab = l.split(" ")
            ag.addApiNode(p, tab(0), tab(1), tab(2))
        }
        ag
    }
  }

  def makeDot(printId : Boolean = false){
    DotPrinter.print(new BufferedWriter(new FileWriter(graph.getCanonicalPath+".dot")),
      ag, JavaNode, printId)
  }

  def solve (trace : Boolean = false,
             decisionMaker : DecisionMaker = DefaultDecisionMaker){
    var inc = 0

    new JavaSolver(accessGraph, decisionMaker).solve(
      if(trace) {() =>
        println("solve iteration " + inc)
        makePng(soutput = Some(new FileOutputStream(
          new File(graph.getCanonicalPath + "_trace" + inc +".png"))))
        inc += 1
      }
      else
        () => ()

    )
  }


  def dot2png(soutput : Option[OutputStream] = None) : Int = {
    val processBuilder = Process(List(
      if(graphvizDot == null) "dot"  // relies on dot directory being in the PATH variable
      else graphvizDot.getCanonicalPath, "-Tpng", graph.getCanonicalPath+".dot"))

    soutput match {
      case None =>(processBuilder #> new File(graph.getCanonicalPath + ".png")).!
      case Some(output) => (processBuilder #> output).!
    }

  }

  def makePng(printId : Boolean = false, soutput : Option[OutputStream] = None) : Int = {
    makeDot(printId)
    dot2png(soutput)
  }

  def parseConstraints() {
      val parser = new ConstraintsParser(accessGraph)
      try {
        parser(new FileReader(decouple))
      } catch {
        case e : NoSuchElementException =>
          accessGraph.discardConstraints()
          throw new AGError("parsing failed :" + e.getLocalizedMessage)

      }
  }

}

object FilesHandler{
  final val defaultDecoupleFileName: String = "decouple.pl"
  final val defaultGraphFileName: String = "graph"
  final val defaultJarListFileName: String = "jar.list"
  final val defaultApiNodesFileName: String = "api_nodes"

  private def defaultFile(dir:File, file: File) =
    new File(dir.getCanonicalFile + File.separator + file)

  def apply(srcDir: File)(jarListFile: File =
                          defaultFile(srcDir, defaultJarListFileName),
                          apiNodesFile :File =
                          defaultFile(srcDir, defaultApiNodesFileName),
                          decouple : File =
                          defaultFile(srcDir, defaultDecoupleFileName),
                          graph : File =
                          defaultFile(srcDir, defaultGraphFileName))
  = new FilesHandler(srcDir.getCanonicalFile, jarListFile, apiNodesFile, decouple, graph)

  def apply() : FilesHandler= apply(new File("."))()

  def findAllJavaFiles(f:File) : List[String] = findAllJavaFiles(List(), f)

  def findAllJavaFiles(res: List[String], f: File) : List[String] = {
    if (f.isDirectory)
      f.listFiles().foldLeft(res)(findAllJavaFiles)
    else {
      if (f.getName.endsWith(".java"))
        f.getPath :: res
      else
        res
    }
  }

  def compile(sources: List[String], jars: List[String]): Option[AST.Program] = {
    val arglist = createArglist(sources, jars, None)
    val f = new AST.Frontend {
      protected override def processWarnings(errors: java.util.Collection[_], unit: AST.CompilationUnit) {
      }
    }
    val br = new AST.BytecodeParser
    val jp = new AST.JavaParser {
      def parse(is: java.io.InputStream, fileName: String): AST.CompilationUnit = {
        (new parser.JavaParser).parse(is, fileName)
      }
    }

    if (f.process(arglist, br, jp)){
      Some(f.getProgram)}
    else
      None
  }

  private[puck] def createArglist(sources: List[String], jars: List[String],
                                  srcdirs:Option[List[String]]): Array[String] = {
    if (jars.length == 0) return sources.toArray

    val args : List[String] = "-classpath" :: jars.mkString("", File.pathSeparator, ".") :: (
      srcdirs match {
        case Some(dirs) =>"-sourcespath" :: dirs.mkString("", ":", ".") :: sources
        case None => sources
      })
    args.toArray
  }

}