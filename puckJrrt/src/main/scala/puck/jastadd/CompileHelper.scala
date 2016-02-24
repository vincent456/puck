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

  def apply(sources: List[String],
            sourcepaths:List[String],
            jars: List[String],
            bootJars : List[String]): Option[Program] = {
      val arglist = createArglist(sources, sourcepaths, jars, bootJars)
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
    val builder = p.buildDependencyGraph(ll)
    builder.addContains(builder.nodesByName("@primitive"), builder.arrayTypeId)
    builder.attachOrphanNodes()
    builder.registerSuperTypes()

    val (_, initialRecord) = NodeMappingInitialState.normalizeNodeTransfos(JavaNodeKind.root.kind,
        builder.g.recording, Seq())

    val g = builder.g.newGraph(recording = Recording())

    (p, g,
      initialRecord,
      builder.nodesByName,
      builder.graph2ASTMap)
  }

  def compileSrcsAndbuildGraph(sources: List[String],
                               sourcepaths:List[String],
                               jars: List[String],
                               bootJars : List[String],
                               decouple : Option[java.io.File] = None) :
    (Program, DependencyGraph, Seq[Transformation], Map[String, NodeId], Map[NodeId, ASTNodeLink]) =
    this.apply(sources, sourcepaths, jars, bootJars) match {
      case None => throw new DGBuildingError("Compilation error, no AST generated")
      case Some(p) => buildGraph(p)
    }



  private[puck] def createArglist(sources: List[String],
                                  sourcepaths:List[String],
                                  jars: List[String],
                                  bootClassPath : List[String]): Array[String] = {

    def prepend(argName : String, argValue : List[String], accu : List[String]) : List[String] =
      if(argValue.isEmpty) accu
      else argName :: argValue.mkString(File.pathSeparator) :: accu

    val args0 = prepend("-classpath", jars, sources)
    val args1 = prepend("-sourcepath", sourcepaths, args0)
    prepend("-bootclasspath", bootClassPath, args1).toArray
  }


}
