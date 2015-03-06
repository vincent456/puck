package puck.javaGraph

import java.io.File

import puck.graph.constraints.{Solver, DecisionMaker}
import puck.graph.constraints.search.SolverBuilder
import puck.graph._
import puck.graph.io.{ConstraintSolvingSearchEngineBuilder, FilesHandler}
import puck.graph.transformations.NodeMappingInitialState
import puck.util.PuckLog._


/**
 * Created by lorilan on 11/08/14.
 */

object JavaSolverBuilder extends SolverBuilder{
  def apply(graph : DependencyGraph,
            dm : DecisionMaker,
            automaticConstraintLoosening : Boolean) : Solver = JavaSolver(graph, dm, automaticConstraintLoosening)
}

class JavaFilesHandler (workingDirectory : File) extends FilesHandler(workingDirectory) {
  def this() = this(new File("."))

  val srcSuffix = ".java"

  var initialRecord : Recording = _

  var sProgram : Option[AST.Program] = None

  //override var graphBuilder : JavaGraphBuilder = _
  var jgraphBuilder : JavaGraphBuilder = _

  def loadGraph(ll : AST.LoadingListener = null) : DependencyGraph = {
    import puck.util.FileHelper.{fileLines, findAllFiles, initStringLiteralsMap}

    val sProg = puck.util.Time.time(logger, defaultVerbosity) {
        logger.writeln("Compiling sources ...")
        val sources = findAllFiles(this.srcDirectory.get, srcSuffix, this.outDirectory.get.getName)
        val jars = findAllFiles(this.srcDirectory.get, ".jar", this.outDirectory.get.getName)
        CompileHelper(sources, fileLines(jarListFile.get) ++: jars )

    }

    val g = puck.util.Time.time(logger, defaultVerbosity) {
      logger.writeln("Building Access Graph ...")
      sProg match {
        case None => throw new AGBuildingError("Compilation error, no AST generated")
        case Some(p) => sProgram = Some(p)

          jgraphBuilder = p.buildAccessGraph(initStringLiteralsMap(decouple.get), ll)

          jgraphBuilder.attachOrphanNodes()

          graphBuilder = jgraphBuilder

          graph = (graphBuilder.g withLogger this.logger).newGraph(nRecording = Recording())

          val (_, transfos) = NodeMappingInitialState.normalizeNodeTransfos(graphBuilder.g.recording(), Seq())
          initialRecord = new Recording(transfos)

          graph
      }
    }

    val (numClass, numItc) = g.nodes.foldLeft((0,0)){ case ((numClass, numItc), n) =>
      val numClass1 = if(n.kind == nodeKind.Class) numClass + 1
          else numClass
        val numItc1 = if(n.kind == nodeKind.Interface) numItc + 1
        else numItc
      (numClass1, numItc1)

    }
    logger.writeln( numClass + " classes and " + numItc + " interfaces parsed")

    g
  }

  val dotHelper = JavaNode

  /*def decisionMaker() = new JavaDefaultDecisionMaker(graph)

  def solver(dm : DecisionMaker[JavaNodeKind]) =
    new JavaSolver(graph, dm)*/

  def applyChangeOnProgram(result : ResultT) : Unit = {

    logger.writeln("applying change !")

    val record = recordOfResult(result)
    val program = sProgram.get
    val applyer = new AG2AST(program, logger)

    record.foldRight((graphOfResult(result), graph, jgraphBuilder.graph2ASTMap)) {
      case (r, (resultGraph, reenactor, graph2ASTMap)) =>
        /*import ShowDG._
        println(showTransformation(resultGraph).shows(r))*/

        val jreenactor = reenactor.asInstanceOf[JavaDependencyGraph]
        val res = applyer(resultGraph, jreenactor, graph2ASTMap, r)

        //println(program)
        (resultGraph, r.redo(reenactor), res)
    }

    println(program)
    println(program.getNumCuFromSources + " before flush")
    program.flushCaches()
    println(program.getNumCuFromSources + " after flush")
    program.eliminateLockedNamesInSources()
    println(program.getNumCuFromSources + " after unlock")
    println(program)

  }

  def printCode() : Unit = {
    sProgram match {
      case Some(p) =>p.printCodeInDirectory(outDirectory.get)
      case None => logger.writeln("no program registered")
    }

  }

  override def searchingStrategies: Seq[ConstraintSolvingSearchEngineBuilder] =
    List(JavaTryAllCSSEBuilder,
      JavaFunneledCSSEBuilder,
      //JavaGradedCSSEBuilder,
      JavaFindFirstCSSEBuilder)
}


object JavaFilesHandler {

  def apply() = new JavaFilesHandler()
  def apply(file : java.io.File) = new JavaFilesHandler(file)



}