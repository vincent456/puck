package puck.javaGraph

import java.io.File

import puck.graph._
import puck.graph.transformations.{NodeMappingInitialState, Recording}
import puck.util.FileHelper

import scala.concurrent.Future


/**
 * Created by lorilan on 22/02/15.
 */
object CompileHelper {

  def apply(sources: List[String], jars: List[String]): Option[AST.Program] = {
    val arglist = createArglist(sources, jars, List())
    val f = new AST.Frontend {
      protected override def processWarnings(errors: java.util.Collection[_], unit: AST.CompilationUnit) : Unit =  {
      }
    }
    val br = new AST.BytecodeParser
    val jp = new AST.JavaParser {
      def parse(is: java.io.InputStream, fileName: String) : AST.CompilationUnit = {
        (new parser.JavaParser).parse(is, fileName)
      }
    }

    if (f.process(arglist, br, jp)){
      Some(f.getProgram)}
    else
      None
  }

  def buildGraph(sources: List[String],
                 jars: List[String],
                 decouple : Option[java.io.File] = None) :
    (AST.Program, DependencyGraph, Recording, Map[String, NodeId], Map[NodeId, ASTNodeLink]) =
    this.apply(sources, jars) match {
      case None => throw new AGBuildingError("Compilation error, no AST generated")
      case Some(p) =>
        val map = decouple match {
          case Some(f) => FileHelper.initStringLiteralsMap(decouple.get)
          case None => new java.util.HashMap[String, java.util.Collection[AST.BodyDecl]]()
        }

        val builder = p.buildAccessGraph(map, null)
        builder.attachOrphanNodes()
        builder.registerSuperTypes()

        val (_, transfos) = NodeMappingInitialState.normalizeNodeTransfos(builder.g.recording(), Seq())
        (p, builder.g.newGraph(nRecording = Recording()),
          new Recording(transfos), builder.nodesByName,
          builder.graph2ASTMap)
    }



  private[puck] def createArglist(sources: List[String],
                                  jars: List[String],
                                  srcdirs:List[String]): Array[String] = {

    if (jars.isEmpty) sources.toArray
    else {
      val args: List[String] = "-classpath" :: jars.mkString("", File.pathSeparator, File.pathSeparator + ".") :: (
        if (srcdirs.isEmpty) sources
        else "-sourcepath" :: srcdirs.mkString("", File.pathSeparator, File.pathSeparator + ".") :: sources)
      args.toArray
    }
  }
}
