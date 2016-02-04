package puck
package graph
package io

import java.io._

import puck.graph.constraints.{ConstraintsParser, ConstraintsMaps}
import puck.graph.transformations.Transformation
import puck.util._

import scala.sys.process.Process

object FilesHandler{
  object Default{
    val srcDirName : String = "src"
    val outDirName : String = "out"
    val decoupleFileName: String = "decouple.pl"
    val graphFileName: String = "graph"
    val jarListFileName: String = "jar.list"
    val apiNodesFileName: String = "api_nodes"
    val logFileName: String = outDirName + File.separator + "graph_solving.log"
  }
}

trait DG2ASTBuilder{
  def apply(srcDirectory : File,
            outDirectory : Option[File],
            jarListFile : Option[File],
            javaRuntime : Option[File],
            logger : PuckLogger,
            ll : LoadingListener = null) : DG2AST
}

trait DG2AST {
  def apply(graph : DependencyGraph)(implicit logger : PuckLogger) : Unit
  def printCode(dir : File)(implicit logger : PuckLogger) : Unit
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

  def this(f : File) = this(Some(f))

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


  val srcDirectory = new FileOption()

  val outDirectory = new FileOption()

  val jarListFile = new FileOption()

  val apiNodesFile = new FileOption()

  val decouple = new FileOption()

  val graphvizDot = new FileOption()

  val editor = new FileOption()

  val logFile = new FileOption()

  //val javaRuntime = new FileOption()
  val javaRuntime = new FileOption(new File("/home/lorilan/jre1.5.0_22/lib/rt.jar"))
  //val javaRuntime = new FileOption(new File("/home/lorilan/jre1.6.0_45/lib/rt.jar"))

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
      javaRuntime.toOption,
      logger, ll.orNull)
  }

  def parseConstraints
  ( dg2ast: DG2AST )
  ( implicit logger : PuckLogger) : Option[ConstraintsMaps] = {
    decouple.get match{
      case None =>
        logger.writeln("cannot parse : no decouple file given")((PuckLog.NoSpecialContext, PuckLog.Error))
        None
      case Some(f) =>
        logger.writeln("parsing " + f)
        try Some(ConstraintsParser(dg2ast.nodesByName, new FileReader(f)))
        catch {
          case t : Throwable =>
            logger.writeln("parsing failed : " + t.getMessage)((PuckLog.NoSpecialContext, PuckLog.Error))
            None
        }
    }
  }

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