package puck
package graph
package io

import java.io._

import puck.graph.constraints.{ConstraintsParser, ConstraintsMaps}
import puck.graph.transformations.Transformation
import puck.util._
import FileHelper._

import scala.sys.process.Process

object FilesHandler{
  object Default{
    val srcDirName : String = "src"
    val outDirName : String = "out"
    val decoupleFileName: String = "decouple.pl"
    val jarListFileName: String = "jar.list"
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

class FilesHandler
( val workingDirectory : File,
  val srcSuffix : String,
  val dG2ASTBuilder: DG2ASTBuilder){



  def fromOutDir : FilesHandler =
    new FilesHandler(outDirectory !, srcSuffix, dG2ASTBuilder)


  import PuckLog.defaultVerbosity

  val logPolicy : PuckLog.Verbosity => Boolean = {
    case _ => true
  }

  type GraphT = DependencyGraph

  var graphBuilder : GraphBuilder = _

  def setDefaultValues(projectRoot : File): Unit = {
    def defaultFile(fileName: String) =
      Some(new File( projectRoot + File.separator + fileName))

    import FilesHandler.Default

    val Some(od) = defaultFile(Default.outDirName)
    if(!od.exists()){
      od.mkdir()
    }
    outDirectory set Some(od)
    jarListFile set defaultFile(Default.jarListFileName)
    decouple set defaultFile(Default.decoupleFileName)
    logFile set defaultFile(Default.logFileName)
  }

  def setWorkingDirectory(dir : File) : Unit = {
    srcDirectory set Some(dir)
    srcDirectory.get match {
      case None => throw new DGError("Invalid working directory !!!")
      case Some(d) => setDefaultValues(d)
    }
  }


  val srcDirectory = new FileOption()

  val outDirectory = new FileOption()

  val jarListFile = new FileOption()

  val decouple = new FileOption()

  val graphvizDot = new FileOption()

  val editor = new FileOption()

  val logFile = new FileOption()

  val javaRuntime = new FileOption()
  //val javaRuntime = new FileOption(new File("/home/lorilan/jre1.5.0_22/lib/rt.jar"))
  //val javaRuntime = new FileOption(new File("/home/lorilan/jre1.6.0_45/lib/rt.jar"))

  if(workingDirectory \ "puck.xml" exists())
    ConfigParser(this)
  else
    setWorkingDirectory(workingDirectory)

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