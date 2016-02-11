package puck
package graph
package io

import java.io._

import puck.graph.constraints.{ConstraintsParser, ConstraintsMaps}
import puck.graph.transformations.Transformation
import puck.util.HMap.StringKey
import puck.util._

object Project{

  import HMap.StringKey

  type Config = HMap[StringKey]
  def emptyConf : Config = HMap.empty

  type FileKey = StringKey[String]
  type FileListKey = StringKey[List[String]]

  object Keys {
    val workspace : FileKey = StringKey("workspace")

    val srcs : FileListKey = StringKey("src")
    val sourcepaths : FileListKey = StringKey("sourcepath")
    val classpath : FileListKey = StringKey("classpath")
    val bootclasspath : FileListKey = StringKey("bootclasspath")

    val out : FileKey = StringKey("out")
    val decouple : FileKey = StringKey("decouple")
    val log : FileKey =  StringKey("log")

    val dotPath : FileKey =  StringKey("dot-path")
    val editor : FileKey =  StringKey("editor")

  }

  val singleValueKeys : List[FileKey] = {
    import Keys._
    List(out, decouple, log)
  }
  val listValueKeys : List[FileListKey] = {
    import Keys._
    List(srcs, sourcepaths, classpath, bootclasspath)
  }


  def apply(seed : File, dG2ASTBuilder: DG2ASTBuilder) : Project =
    new Project(ConfigParser(seed), dG2ASTBuilder)

  object Default {

    val configFile : String = "puck.xml"
    val srcRoot : String = "src"
    val out : String = "out"
    val classpathRoot : String = "lib"
    val decouple: String = "decouple.wld"
    val log: String = out + File.separator + "log.txt"
  }
}
import Project._

trait DG2ASTBuilder{
  def apply(fh : Project,
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

class Project
(var config : Project.Config,
 val dG2ASTBuilder: DG2ASTBuilder){

  def apply[T](key : StringKey[T])  : Option[T] = config get key
  def set(k : FileKey, f : File) : Unit =
    config = config put (k, f.getAbsolutePath)

  def fileList(k : FileListKey ) : List[String] = config getOrElse (k, List())


  def fromOutDir : Project =
    new Project(config, dG2ASTBuilder)


  import PuckLog.defaultVerbosity

  type GraphT = DependencyGraph

  var graphBuilder : GraphBuilder = _


//  import puck.util.FileHelper.FileOps
//  def setDefaultValues(projectRoot : File): Unit = {
//
//    val Some(od) = Some(projectRoot \ Default.outDirName)
//    if(!od.exists()){
//      od.mkdir()
//    }
//
//    outDirectory set Some(od)
//    decouple set Some(projectRoot \ Default.decoupleFileName)
//    logFile set Some(projectRoot \ Default.logFileName)
//  }

  def workspace : File =
    new File(config get Keys.workspace getOrElse ".")

  def someFile(k : FileKey) : Option[File] =
    config get k map (new File(_))

  def decouple = someFile(Keys.decouple)

  def outDirectory = someFile(Keys.out)

  def graphvizDot = someFile(Keys.dotPath)

  def editor = someFile(Keys.editor)

  def logFile = someFile(Keys.log)



  def loadGraph
  ( ll : Option[LoadingListener] = None)
  ( implicit logger : PuckLogger) : DG2AST = {
     dG2ASTBuilder(this, logger, ll.orNull)
  }

  def parseConstraints
  ( dg2ast: DG2AST )
  ( implicit logger : PuckLogger) : Option[ConstraintsMaps] = {
    decouple match{
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

//  private def openList(files : Seq[String]) : Unit = {
//    val ed = editor.get match {
//      case None => sys.env("EDITOR")
//      case Some(f) => f.getCanonicalPath
//    }
//    Process(ed  +: files ).!;()
//  }
//
//  import puck.util.FileHelper.findAllFiles
//
//  def openSources() = openList(findAllFiles(srcDirectory !, srcSuffix,
//    outDirectory.toOption map (_.getName)))
//  def openProduction() = openList(findAllFiles(outDirectory !, srcSuffix,
//    outDirectory.toOption map (_.getName)))

}