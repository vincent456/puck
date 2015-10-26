package puck.jastadd

import java.io.{File, InputStream}
import java.util.Iterator

import puck.graph.transformations.{NodeMappingInitialState, Recording, Transformation}
import puck.graph.{DGBuildingError, DependencyGraph, NodeId}
import puck.javaGraph.nodeKind.JavaNodeKind

object CompileHelper {

  def apply(sources: List[String], jars: List[String]): Option[AST.Program] = {
      val arglist = createArglist(sources, jars, List())

      val f = new AST.Frontend {
        protected override def processErrors(errors: java.util.Collection[_], unit: AST.CompilationUnit): Unit =  {
          System.err.println("Errors:")

            val it: Iterator[_] = errors.iterator
            while (it.hasNext) {
              val i = it.next()
              System.err.println(i.getClass)
              System.err.println(i)
            }

        }
        protected override def processWarnings(errors: java.util.Collection[_], unit: AST.CompilationUnit): Unit = {
        }
      }
      val br = new AST.BytecodeParser
      val jp = new AST.JavaParser {
        def parse(is: InputStream, fileName: String): AST.CompilationUnit = {
          (new parser.JavaParser).parse(is, fileName)
        }
      }

    if (f.process(arglist, br, jp)){
      Some(f.getProgram)}
    else
      None
  }

  def addVoid(p : AST.Program, builder : JastaddGraphBuilder) : Unit = {
    val voidASTNode = p.findType("void")
    val voidDGNode = builder.addApiTypeNode(voidASTNode)
    builder.registerDecl(voidDGNode, voidASTNode)
  }

  def buildGraph(p : AST.Program,
                 ll : puck.LoadingListener = null )  :
  (AST.Program, DependencyGraph, Seq[Transformation], Map[String, NodeId], Map[NodeId, ASTNodeLink]) = {

    val builder = p.buildDependencyGraph(null, ll)
    addVoid(p, builder)
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
    (AST.Program, DependencyGraph, Seq[Transformation], Map[String, NodeId], Map[NodeId, ASTNodeLink]) =
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
      args.toArray
    }
  }


}
