package puck.javaAG

import puck.graph.backTrack.Recording
import puck.graph.constraints.search._
import puck.graph.{AGNode, AccessGraph}
import puck.graph.constraints._
import puck.graph.io.ConstraintSolvingSearchEngineBuilder
import puck.javaAG.nodeKind._
import puck.search.{SearchState, SearchEngine}
import puck.util.Logger

/**
 * Created by lorilan on 12/09/14.
 */
//CSSE : Constraint Solving Search Engine
trait JavaCSSEBuilder
  extends ConstraintSolvingSearchEngineBuilder[JavaNodeKind]{
  val violationsKindPriority = List[JavaNodeKind](Field(), Constructor(), Class(), Interface())
}

object JavaSolverBuilder extends SolverBuilder[JavaNodeKind]{
  def apply(graph : AccessGraph[JavaNodeKind],
            logger : Logger[Int],
            dm : DecisionMaker[JavaNodeKind]) = new JavaSolver(graph, logger, dm)
}

object JavaTryAllCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Try all"

  def apply( searchEngineLogger : Logger[Int],
             solverLogger : Logger[Int],
             graph : AccessGraph[JavaNodeKind]) : SearchEngine[Recording[JavaNodeKind]] =
    new TryAllCSSE(searchEngineLogger,
      violationsKindPriority,
      graph, JavaSolverBuilder, solverLogger)
}

/*object JavaGradedCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Best graded next state"

  def apply( searchEngineLogger : Logger[Int],
             solverLogger : Logger[Int],
             graph : AccessGraph[JavaNodeKind],
             printTrace : SearchState[ConstraintSolvingNodesChoice[JavaNodeKind],
               Option[AGNode[JavaNodeKind]]] => Unit) : SearchEngine[ConstraintSolvingNodesChoice[JavaNodeKind],
    Option[AGNode[JavaNodeKind]]] =
    new GradedCSSE(searchEngineLogger,
      violationsKindPriority,
      graph, JavaSolverBuilder, solverLogger, printTrace)
}*/

object JavaFindFirstCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "First solution"

  def apply( searchEngineLogger : Logger[Int],
             solverLogger : Logger[Int],
             graph : AccessGraph[JavaNodeKind]) : SearchEngine[Recording[JavaNodeKind]] =
    new FindFirstCSSE(searchEngineLogger,
      violationsKindPriority,
      graph, JavaSolverBuilder, solverLogger)
}
