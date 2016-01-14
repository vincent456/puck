package puck.jastadd

import java.io.{File, InputStream}
import java.util.Iterator

import puck.graph.comparison.NodeMappingInitialState
import puck.graph.transformations.{Recording, Transformation}
import puck.graph.{DGBuildingError, DependencyGraph, NodeId}
import puck.javaGraph.nodeKind.JavaNodeKind
import org.extendj.ast.{List => ASTList, _}
import org.extendj.parser


object CompileHelper {

  def apply(sources: List[String], jars: List[String]): Option[Program] = {
      val arglist = createArglist(sources, jars, List())

      val f = new Frontend {
//        protected override def processErrors(errors: java.util.Collection[Problem], unit: CompilationUnit): Unit =  {
//          System.err.println("Errors:")
//
//            val it: Iterator[_] = errors.iterator
//            while (it.hasNext) {
//              val i = it.next()
//              System.err.println(i)
//            }
//
//        }
        protected override def processWarnings(errors: java.util.Collection[Problem], unit: CompilationUnit): Unit = {
        }
      }
      val br = new BytecodeReader() {
        def read(is: InputStream, fullName: String, p: Program) : CompilationUnit = {
          new BytecodeParser(is, fullName).parse(null, null, p)
        }
      }

      val jp = new JavaParser() {
        override def parse(is: InputStream, fileName: String) : CompilationUnit = {
          new parser.JavaParser().parse(is, fileName)
        }
      }

    if (f.run(arglist, br, jp) == 0){
      Some(f.getProgram)}
    else
      None
  }

  def buildGraph(p : Program,
                 ll : puck.LoadingListener = null )  :
  (Program, DependencyGraph, Seq[Transformation], Map[String, NodeId], Map[NodeId, ASTNodeLink]) = {
    val builder = p.buildDependencyGraph(null, ll)
    builder.attachOrphanNodes()
    builder.registerSuperTypes()

    val (_, initialRecord) = NodeMappingInitialState.normalizeNodeTransfos(JavaNodeKind.rootKind,
        builder.g.recording, Seq())

    val g = builder.g.newGraph(recording = Recording())

    (p, g,
      initialRecord,
      builder.nodesByName,
      builder.graph2ASTMap)
  }

  def compileSrcsAndbuildGraph(sources: List[String],
                 jars: List[String],
                 decouple : Option[java.io.File] = None) :
    (Program, DependencyGraph, Seq[Transformation], Map[String, NodeId], Map[NodeId, ASTNodeLink]) =
    this.apply(sources, jars) match {
      case None => throw new DGBuildingError("Compilation error, no AST generated")
      case Some(p) => buildGraph(p)
    }



  private[puck] def createArglist(sources: List[String],
                                  jars: List[String],
                                  srcdirs:List[String]): Array[String] = {

    if (jars.isEmpty) sources.toArray
    else {
      val args: List[String] = "-classpath" :: jars.mkString("", File.pathSeparator, File.pathSeparator + ".") :: (
        if (srcdirs.isEmpty) sources
        else "-sourcepath" :: srcdirs.mkString("", File.pathSeparator, File.pathSeparator + ".") :: sources)
      //( "-bootclasspath" :: "/home/lorilan/jre1.6.0_45/lib/rt.jar" :: args).toArray
        args.toArray
    }
  }


}
