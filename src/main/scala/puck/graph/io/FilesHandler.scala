package puck.graph
package io

import java.io._

import puck.graph.constraints.search.SolverBuilder
import puck.graph.transformations.{Transformation, TransformationRules, Recording}
import puck.javaGraph.ASTNodeLink
import puck.search.{Search, SearchState, SearchEngine}
import puck.util._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.sys.process.Process
import scala.util.{Success, Try}

trait ConstraintSolvingSearchEngineBuilder {
  def apply(initialRecord : Seq[Transformation], graph : DependencyGraph,
  automaticConstraintLoosening : Boolean) :
  SearchEngine[ResultT]
}

object FilesHandler{
  object Default{
    final val srcDirName : String = "src"
    final val outDirName : String = "out"
    final val decoupleFileName: String = "decouple.pl"
    final val graphFileName: String = "graph"
    final val jarListFileName: String = "jar.list"
    final val apiNodesFileName: String = "api_nodes"
    final val logFileName: String = outDirName + File.separator + "graph_solving.log"
  }

  def makeDot(graph : DependencyGraph, dotHelper: DotHelper,
              printingOptions: PrintingOptions,
              writer : OutputStreamWriter) : Unit = {
    val printer = new DotPrinter(new BufferedWriter(writer), graph, dotHelper, printingOptions)
    printer()
  }

  def makeImage(graphvizDot : Option[File], dotHelper: DotHelper, pathWithoutSuffix : String)
                   (graph : DependencyGraph,
                    printingOptions: PrintingOptions,
                    sOutput : Option[OutputStream] = None,
                    outputFormat : DotOutputFormat = Png)
                   (finish : Try[Int] => Unit = {case _ => ()}) : Unit = {

    //TODO fix bug when chaining the two function with a pipe
    FilesHandler.makeDot(graph, dotHelper, printingOptions, new FileWriter(pathWithoutSuffix + ".dot"))

    finish(Success(convertDot(graphvizDot, pathWithoutSuffix, sInput = None, sOutput, outputFormat)))

    /*val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    Future {
      convertDot(graphvizDot, pathWithoutSuffix, sInput = Some(pipedInput), sOutput, outputFormat)
    } onComplete finish

    makeDot(graph, dotHelper, printingOptions, writer = new OutputStreamWriter(pipedOutput))*/
  }

  def convertDot( graphvizDot : Option[File],
                  pathWithoutSuffix : String,
                  sInput : Option[InputStream] = None,
                  sOutput : Option[OutputStream] = None,
                  outputFormat : DotOutputFormat) : Int = {

    val dot = graphvizDot match {
      case None => "dot" // relies on dot directory being in the PATH variable
      case Some(f) => f.getCanonicalPath
    }

    val processBuilder =
      sInput match {
        case None => Process(List(dot,
          "-T" + outputFormat, pathWithoutSuffix + ".dot"))
        case Some(input) => Process(List(dot,
          "-T" + outputFormat)) #< input
      }

    sOutput match {
      case None =>(processBuilder #> new File( pathWithoutSuffix + "." + outputFormat)).!
      case Some(output) =>(processBuilder #> output).!
    }
  }


}

/*trait ConstraintSolvingSearchEngineBuilder[Kind <: NodeKind[Kind]] {
  def apply(graph : AccessGraph[Kind]) :
  SearchEngine[Recording[Kind]]
}*/


trait DG2ASTBuilder{
  def apply(srcDirectory : File,
            outDirectory : File,
            jarListFile : File,
            logger : PuckLogger,
            ll : AST.LoadingListener = null) : DG2AST
}

trait DG2AST {
  def apply(res : ResultT) : Unit
  def printCode(dir : File) : Unit
  def parseConstraints(decouple : File) : DG2AST
  def initialGraph : DependencyGraph
  def initialRecord : Seq[Transformation]
  def nodesByName : Map[String, NodeId]
  def astNodeOf(graph : DependencyGraph, id : NodeId) : ASTNodeLink
}

class FilesHandler
(val workingDirectory : File,
 val solverBuilder : SolverBuilder,
 //TODO ? change to List[String] ?
 val srcSuffix : String,
 val dotHelper : DotHelper,
//TODO see if it can be placed somewhere else
 val transformationRules : TransformationRules,
 val dG2ASTBuilder: DG2ASTBuilder,
 val searchingStrategies : Seq[ConstraintSolvingSearchEngineBuilder]){


  var dg2ast: DG2AST = _

  private [this] var srcDir0 : Option[File] = None
  private [this] var outDir0 : Option[File] = None
  private [this] var jarListFile0 : Option[File] = None
  private [this] var apiNodesFile0 : Option[File] = None
  private [this] var decouple0 : Option[File] = None
  private [this] var logFile0 : Option[File] = None


  var graphStubFileName : String = FilesHandler.Default.graphFileName

  import PuckLog.defaultVerbosity

  val logPolicy : PuckLog.Verbosity => Boolean = {
/*    case (PuckLog.Search,_) | (PuckLog.Solver, _) => true
    case (PuckLog.InGraph,_) | (PuckLog.InJavaGraph, _ ) => true
    case (PuckLog.GraphTransfoRules, _) => true*/
    /*case (PuckLog.NoSpecialContext, _)
         | (PuckLog.Solver, PuckLog.Debug)
         | (PuckLog.InGraph, PuckLog.Debug) => true*/
    /*case (PuckLog.GraphComparisonSearch, _) => true*/
    case _ => true
  }

  def logger : PuckLogger = logger0
  def logger_=( l : PuckLogger): Unit = {logger0 = l}

  type GraphT = DependencyGraph

  //private [this] var ag : GraphT = _
  def graph = dg2ast.initialGraph
  //protected def graph_=(g : GraphT): Unit = { ag = g }

  def initialRecord : Seq[Transformation] = dg2ast.initialRecord

  var graphBuilder : GraphBuilder = _

  def setCanonicalOptionFile(prev : Option[File], sf : Option[File]) = {
    sf match {
      case None => prev
      case Some(f) => val fc = f.getCanonicalFile
        if(fc.exists())
          Some(fc)
        else {
          logger.writeln("%s does not exists.".format(f))
          None
        }
    }
  }


  def setWorkingDirectory(dir : File) : Unit = {
    this.srcDir0 = setCanonicalOptionFile(this.srcDir0, Some(dir))
    this.srcDir0 match {
      case None => throw new DGError("Invalid working directory !!!")
      case Some(d) =>

        def defaultFile(fileName: String) =
          Some(new File( d + File.separator + fileName))

        import FilesHandler.Default

        val Some(od) = defaultFile(Default.outDirName)
        if(!od.exists()){
          od.mkdir()
        }
        outDir0 = Some(od)
        jarListFile0 = defaultFile(Default.jarListFileName)
        apiNodesFile0 = defaultFile(Default.apiNodesFileName)
        decouple0 = defaultFile(Default.decoupleFileName)
        logFile0 = defaultFile(Default.logFileName)
    }
  }

  setWorkingDirectory(workingDirectory)

  /*private [this] var logger0 : PuckLogger = logFile match {
    case None => new PuckSystemLogger(logPolicy)
    case Some(f) => new PuckFileLogger (logPolicy, f)
  }*/
  private [this] var logger0 : PuckLogger = new PuckSystemLogger(logPolicy)


  def srcDirectory = this.srcDir0
  def srcDirectory_=(sdir : Option[File]) : Unit = {
    this.srcDir0 = setCanonicalOptionFile(this.srcDir0, sdir)
  }

  def outDirectory = this.outDir0
  def outDirectory_=(sdir : Option[File]) : Unit = {
    this.outDir0 = setCanonicalOptionFile(this.outDir0, sdir)
  }

  def jarListFile = this.jarListFile0
  def jarListFile_=(sf : Option[File]) : Unit = {
    this.jarListFile0 = setCanonicalOptionFile(this.jarListFile0, sf)
  }

  def apiNodesFile = this.apiNodesFile0
  def apiNodesFile_=(sf : Option[File]) : Unit = {
    this.apiNodesFile0 = setCanonicalOptionFile(this.apiNodesFile0, sf)
  }

  def decouple = this.decouple0
  def decouple_=(sf: Option[File]) : Unit = {
    this.decouple0 = setCanonicalOptionFile(this.decouple0, sf)
  }


  private [this] var gdot : Option[File] = None

  def graphvizDot = this.gdot
  def graphvizDot_=(sf: Option[File]) : Unit = {
    this.gdot = setCanonicalOptionFile(this.gdot, sf)
  }

  private [this] var editor0 : Option[File] = None

  def editor = editor0
  def editor_=(sf: Option[File]): Unit = {
    this.editor0 = setCanonicalOptionFile(this.editor0, sf)
  }

  def logFile = logFile0

  def graphFilePath : String = outDirectory match {
    case None => throw new DGError("no output directory !!")
    case Some(d) => d + File.separator + graphStubFileName
  }

  def graphFile(suffix : String) : File = new File(graphFilePath + suffix)



  def loadGraph(ll : AST.LoadingListener) : Unit = {
    dg2ast = dG2ASTBuilder(srcDirectory.get,
                           outDirectory.get,
                           jarListFile.get, logger)
  }

  /*def makeProlog(){
    PrologPrinter.print(new BufferedWriter(new FileWriter(graphFile(".pl"))), ag)
  }*/

  def parseConstraints() : Unit = {
    decouple match{
      case None => throw new DGError("cannot parse : no decouple file given")
      case Some(f) =>
        logger.writeln("parsing " + f)
        dg2ast = dg2ast.parseConstraints(f)
    }

  }

  type ST = SearchState[ResultT]

  def explore (trace : Boolean = false,
               builder : ConstraintSolvingSearchEngineBuilder,
               automaticConstraintLoosening: Boolean) : Search[ResultT] = {

    val engine = builder(initialRecord, graph, automaticConstraintLoosening)

    puck.util.Time.time(logger, defaultVerbosity) {
      engine.explore()
    }

    engine
  }


  def printCSSearchStatesGraph(states : Map[Int, Seq[SearchState[ResultT]]],
                               visibility : VisibilitySet,
                               printId : Boolean,
                               printSignature : Boolean) : Unit = {
    val d = graphFile("_results")
    d.mkdir()
    states.foreach{
      case (cVal, l) =>
        val subDir = graphFile("_results%c%d".format(File.separatorChar, cVal))
        subDir.mkdir()
        printCSSearchStatesGraph(subDir, l, visibility, None, printId, printSignature)
    }
  }

  def printCSSearchStatesGraph(dir : File,
                               states : Seq[SearchState[ResultT]],
                               visibility : VisibilitySet,
                               sPrinter : Option[(SearchState[ResultT] => String)],
                               printId : Boolean,
                               printSignature : Boolean) : Unit = {

    val printer = sPrinter match {
      case Some(p) => p
      case None =>
        s : SearchState[ResultT] => s.uuid()
    }

    states.foreach { s =>
      val graph = graphOfResult(s.result)
      val f = new File("%s%c%s.png".format(dir.getAbsolutePath, File.separatorChar, printer(s)))
      val options = PrintingOptions(visibility, printId, printSignature, None)
      makeImage(graph, options, Some(new FileOutputStream(f)), Png){_ => ()}
    }
  }

  def makeImage = FilesHandler.makeImage(graphvizDot, dotHelper, graphFilePath) _

  def printCode() : Unit = dg2ast.printCode(outDirectory.get)

  def applyChangeOnProgram(record : ResultT) : Unit =
    dg2ast(record)



  private def openList(files : Seq[String]) : Unit = {
    val ed = editor match {
      case None => sys.env("EDITOR")
      case Some(f) => f.getCanonicalPath
    }
    Process(ed  +: files ).!;()
  }

  import puck.util.FileHelper.findAllFiles

  def openSources() = openList(findAllFiles(srcDirectory.get, srcSuffix,
    outDirectory.get.getName))
  def openProduction() = openList(findAllFiles(outDirectory.get, srcSuffix,
    outDirectory.get.getName))

}