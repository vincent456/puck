package puck.javaAG

import java.io.{FileWriter, BufferedWriter, File}

import puck.graph.backTrack.Recording
import puck.graph.backTrack.comparison.NodeMappingInitialState
import puck.graph.constraints.DecisionMaker
import puck.graph.{AGBuildingError, AccessGraph}
import puck.graph.io.{ConstraintSolvingSearchEngineBuilder, FilesHandler}
import puck.javaAG.nodeKind.JavaNodeKind
import puck.util.Logger

/**
 * Created by lorilan on 11/08/14.
 */
class JavaFilesHandler (workingDirectory : File) extends FilesHandler[JavaNodeKind](workingDirectory) {
  def this() = this(new File("."))

  val srcSuffix = ".java"

  def loadGraph(ll : AST.LoadingListener = null) : JavaAccessGraph = {
    import puck.util.FileHelper.{fileLines, initStringLiteralsMap, findAllFiles}

    JavaFilesHandler.compile(findAllFiles(this.srcDirectory.get, srcSuffix,
      this.outDirectory.get.getName),
      fileLines(jarListFile.get)) match {
      case None => throw new AGBuildingError("Compilation error, no AST generated")
      case Some(p) =>
        val jgraph = p.buildAccessGraph(initStringLiteralsMap(decouple.get), ll)
        fileLines(apiNodesFile.get).foreach {
          (l: String) =>
            val tab = l.split(" ")
            jgraph.addApiNode(p, tab(0), tab(1), tab(2))
        }
        graph = jgraph

        val (_, tranfos) = NodeMappingInitialState.normalizeNodeTransfos(jgraph.transformations.recording.composition)

        graph.initialRecord = tranfos
        //create a new caretaker for the comming transformations
        graph.transformations.stopRegister()
        graph.transformations.startRegister()

        jgraph
    }
  }

  val dotHelper = JavaNode

  def decisionMaker() = new JavaDefaultDecisionMaker(graph)

  def solver(dm : DecisionMaker[JavaNodeKind], logger: Logger[Int]) =
    new JavaSolver(graph, logger, dm)

  def printCode() {
    graph.asInstanceOf[JavaAccessGraph].program.printCodeInDirectory(outDirectory.get)
  }

  override def searchingStrategies: List[ConstraintSolvingSearchEngineBuilder[JavaNodeKind]] =
    List(JavaTryAllCSSEBuilder,
      //JavaGradedCSSEBuilder,
    JavaFindFirstCSSEBuilder)
}


object JavaFilesHandler{

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