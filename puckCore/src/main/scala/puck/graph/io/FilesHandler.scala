package puck
package graph
package io

import java.io._

import puck.graph.constraints.{Solver, DecisionMaker}
import puck.graph.transformations.Transformation
import puck.util._

import scala.sys.process.Process

import scala.concurrent.ExecutionContext.Implicits.global

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
  def apply(graph : DependencyGraph)(implicit logger : PuckLogger) : Unit
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
 val dG2ASTBuilder: DG2ASTBuilder){

  def fromOutDir : FilesHandler =
    new FilesHandler(outDirectory !, srcSuffix, dG2ASTBuilder)


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
  ( ll : Option[LoadingListener] = None)
  ( implicit logger : PuckLogger) : DG2AST = {
     dG2ASTBuilder(
      srcDirectory !,
      outDirectory.toOption,
      jarListFile,
      logger, ll.orNull)
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

  /*def printCSSearchStatesGraph
  ( states : Map[Int, Seq[SearchState[SResult]]],
    dotHelper : DotHelper,
    visibility : VisibilitySet.T,
    printId : Boolean,
    printSignature : Boolean) : Unit = {
    val d = graphFile("_results")
    d.mkdir()
    states.foreach{
      case (cVal, l) =>
        val subDir = graphFile("_results%c%d".format(File.separatorChar, cVal))
        subDir.mkdir()
        printCSSearchStatesGraph(subDir, l, dotHelper, visibility, None, printId, printSignature)
    }
  }

  def printCSSearchStatesGraph
  ( dir : File,
    states : Seq[SearchState[SResult]],
    dotHelper : DotHelper,
    visibility : VisibilitySet.T,
    sPrinter : Option[(SearchState[SResult] => String)],
    printId : Boolean,
    printSignature : Boolean) : Unit = {

    val printer = sPrinter match {
      case Some(p) => p
      case None =>
        s : SearchState[_] => s.uuid()
    }

    states.foreach { s =>
      val graph = graphOfResult(s.loggedResult.value)
      val f = new File("%s%c%s.png".format(dir.getAbsolutePath, File.separatorChar, printer(s)))
      val options = PrintingOptions(visibility, printId, printSignature, None)
      DotPrinter.genImage(graph, dotHelper, options, Png, new FileOutputStream(f)){_ => ()}
    }
  }*/

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