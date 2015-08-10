package puck
package graph
package io

import java.io._

import puck.graph.constraints.{Solver, DecisionMaker}
import puck.graph.transformations.Transformation
import puck.search._
import puck.util._

import scala.sys.process.Process
import scala.util.{Success, Try}


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

  type AutoConstraintLoosening = Boolean
  type SolverBuilder = (DecisionMaker, AutoConstraintLoosening) => Solver

}

/*trait ConstraintSolvingSearchEngineBuilder[Kind <: NodeKind[Kind]] {
  def apply(graph : AccessGraph[Kind]) :
  SearchEngine[Recording[Kind]]
}*/


trait DG2ASTBuilder{
  def apply(srcDirectory : File,
            outDirectory : Option[File],
            jarListFile : Option[File],
            logger : PuckLogger,
            ll : LoadingListener = null) : DG2AST
}

trait DG2AST {
  def apply(res : DependencyGraph)(implicit logger : PuckLogger) : Unit
  def printCode(dir : File)(implicit logger : PuckLogger) : Unit
  def parseConstraints(decouple : File)(implicit logger : PuckLogger) : DG2AST
  def initialGraph : DependencyGraph
  def initialRecord : Seq[Transformation]
  def nodesByName : Map[String, NodeId]
  def code(graph : DependencyGraph, id : NodeId) : String
}

object FileOption {
  implicit def fileOptionToOptionFile(fo : FileOption) : Option[File] =
    fo.get
}

class FileOption(private [this] var sf : Option[File] = None) {
  def get = sf
  def ! = sf.get
  def set(sf : Option[File]) =
    sf match {
      case None => ()
      case Some(f) => val fc = f.getCanonicalFile
        this.sf =
          if(fc.exists()) Some(fc)
          else None
    }

  def toOption = sf
}

class FilesHandler
(val workingDirectory : File,
 //TODO ? change to List[String] ?
 val srcSuffix : String,
 val dotHelper : DotHelper){

  def fromOutDir : FilesHandler =
    new FilesHandler(outDirectory !, srcSuffix, dotHelper)





  var graphStubFileName : String = FilesHandler.Default.graphFileName

  import PuckLog.defaultVerbosity

  val logPolicy : PuckLog.Verbosity => Boolean = {
    case _ => true
  }

  type GraphT = DependencyGraph

  var graphBuilder : GraphBuilder = _

  def setWorkingDirectory(dir : File) : Unit = {
    srcDirectory set Some(dir)
    srcDirectory.get match {
      case None => throw new DGError("Invalid working directory !!!")
      case Some(d) =>

        def defaultFile(fileName: String) =
          Some(new File( d + File.separator + fileName))

        import FilesHandler.Default

        val Some(od) = defaultFile(Default.outDirName)
        if(!od.exists()){
          od.mkdir()
        }
        outDirectory set Some(od)
        jarListFile set defaultFile(Default.jarListFileName)
        apiNodesFile set defaultFile(Default.apiNodesFileName)
        decouple set defaultFile(Default.decoupleFileName)
        logFile set defaultFile(Default.logFileName)
    }
  }



  /*private [this] var logger0 : PuckLogger = logFile match {
    case None => new PuckSystemLogger(logPolicy)
    case Some(f) => new PuckFileLogger (logPolicy, f)
  }*/



  val srcDirectory = new FileOption()

  val outDirectory = new FileOption()

  val jarListFile = new FileOption()

  val apiNodesFile = new FileOption()

  val decouple = new FileOption()

  val graphvizDot = new FileOption()

  val editor = new FileOption()

  val logFile = new FileOption()

  setWorkingDirectory(workingDirectory)

  def graphFilePath : String = outDirectory.get match {
    case None => throw new DGError("no output directory !!")
    case Some(d) => d + File.separator + graphStubFileName
  }

  def graphFile(suffix : String) : File = new File(graphFilePath + suffix)



  def loadGraph
  ( dG2ASTBuilder : DG2ASTBuilder,
    ll : LoadingListener)
  ( implicit logger : PuckLogger) : DG2AST = {
     dG2ASTBuilder(
      srcDirectory !,
      outDirectory.toOption,
      jarListFile,
      logger, ll)
  }

  /*def makeProlog(){
    PrologPrinter.print(new BufferedWriter(new FileWriter(graphFile(".pl"))), ag)
  }*/

  def parseConstraints
  ( dg2ast: DG2AST )
  ( implicit logger : PuckLogger) : DG2AST = {
    decouple.get match{
      case None => throw new DGError("cannot parse : no decouple file given")
      case Some(f) =>
        logger.writeln("parsing " + f)
        dg2ast.parseConstraints(f)
    }

  }

  def printCSSearchStatesGraph(states : Map[Int, Seq[SearchState[ResultT]]],
                               visibility : VisibilitySet.T,
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
                               visibility : VisibilitySet.T,
                               sPrinter : Option[(SearchState[ResultT] => String)],
                               printId : Boolean,
                               printSignature : Boolean) : Unit = {

    val printer = sPrinter match {
      case Some(p) => p
      case None =>
        s : SearchState[ResultT] => s.uuid()
    }

    states.foreach { s =>
      val graph = graphOfResult(s.loggedResult.value)
      val f = new File("%s%c%s.png".format(dir.getAbsolutePath, File.separatorChar, printer(s)))
      val options = PrintingOptions(visibility, printId, printSignature, None)
      makeImage(graph, options, Some(new FileOutputStream(f)), Png){_ => ()}
    }
  }

  def makeImage = FilesHandler.makeImage(graphvizDot.get, dotHelper, graphFilePath) _

  private def openList(files : Seq[String]) : Unit = {
    val ed = editor.get match {
      case None => sys.env("EDITOR")
      case Some(f) => f.getCanonicalPath
    }
    Process(ed  +: files ).!;()
  }

  import puck.util.FileHelper.findAllFiles

  def openSources() = openList(findAllFiles(srcDirectory !, srcSuffix,
    outDirectory.toOption map (_.getName)))
  def openProduction() = openList(findAllFiles(outDirectory !, srcSuffix,
    outDirectory.toOption map (_.getName)))

}