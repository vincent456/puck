package puck.javaAG

import java.io.File

import puck.graph.constraints.{Solver, DecisionMaker}
import puck.graph.constraints.search.SolverBuilder
import puck.graph.immutable.transformations.{Recording, NodeMappingInitialState, Transformation}
import puck.graph.{AccessGraph, JavaNode, JavaSolver, AGBuildingError}
import puck.graph.io.{ConstraintSolvingSearchEngineBuilder, FilesHandler}


/**
 * Created by lorilan on 11/08/14.
 */

object JavaSolverBuilder extends SolverBuilder{
  def apply(graph : AccessGraph,
            dm : DecisionMaker) : Solver = JavaSolver(graph, dm)
}

class JavaFilesHandler (workingDirectory : File) extends FilesHandler(workingDirectory) {
  def this() = this(new File("."))

  val srcSuffix = ".java"

  var initialRecord : Seq[Transformation] = _

  def loadGraph(ll : AST.LoadingListener = null) : AccessGraph = {
    import puck.util.FileHelper.{fileLines, findAllFiles, initStringLiteralsMap}

    JavaFilesHandler.compile(findAllFiles(this.srcDirectory.get, srcSuffix,
      this.outDirectory.get.getName),
      fileLines(jarListFile.get)) match {
      case None => throw new AGBuildingError("Compilation error, no AST generated")
      case Some(p) =>
        val jGraphBuilder = p.buildAccessGraph(initStringLiteralsMap(decouple.get), ll)
        fileLines(apiNodesFile.get).foreach {
          (l: String) =>
            val tab = l.split(" ")
            jGraphBuilder.addApiNode(p, tab(0), tab(1), tab(2))
        }

        graphBuilder = jGraphBuilder
        graph = (graphBuilder.g withLogger this.logger).newGraph(nRecording = Recording())

        val (_, transfos) = NodeMappingInitialState.normalizeNodeTransfos(graphBuilder.g.recording(), Seq())
        initialRecord = transfos




        graph
    }
  }

  val dotHelper = JavaNode

  /*def decisionMaker() = new JavaDefaultDecisionMaker(graph)

  def solver(dm : DecisionMaker[JavaNodeKind]) =
    new JavaSolver(graph, dm)*/

  /*def printCode() {
    graph.asInstanceOf[JavaAccessGraph].program.printCodeInDirectory(outDirectory.get)
  }*/

  override def searchingStrategies: Seq[ConstraintSolvingSearchEngineBuilder] =
    List(JavaFunneledCSSEBuilder,
      JavaTryAllCSSEBuilder,
      //JavaGradedCSSEBuilder,
      JavaFindFirstCSSEBuilder)
}


object JavaFilesHandler{

  def apply() = new JavaFilesHandler()
  def apply(file : java.io.File) = new JavaFilesHandler(file)

  def compile(sources: List[String], jars: List[String]): Option[AST.Program] = {
    val arglist = createArglist(sources, jars, None)
    val f = new AST.Frontend {
      protected override def processWarnings(errors: java.util.Collection[_], unit: AST.CompilationUnit) {
      }
    }
    val br = new AST.BytecodeParser
    val jp = new AST.JavaParser {
      def parse(is: java.io.InputStream, fileName: String): AST.CompilationUnit = {
        (new parser.JavaParser).parse(is, fileName)
      }
    }

    if (f.process(arglist, br, jp)){
      Some(f.getProgram)}
    else
      None
  }

  private[puck] def createArglist(sources: List[String], jars: List[String],
                                  srcdirs:Option[List[String]]): Array[String] = {
    if (jars.length == 0) return sources.toArray

    val args : List[String] = "-classpath" :: jars.mkString("", File.pathSeparator, ".") :: (
      srcdirs match {
        case Some(dirs) =>"-sourcespath" :: dirs.mkString("", ":", ".") :: sources
        case None => sources
      })
    args.toArray
  }

}