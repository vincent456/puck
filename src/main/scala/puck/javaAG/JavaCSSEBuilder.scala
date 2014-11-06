package puck.javaAG

import puck.graph.constraints.search.{TryAllCSSE, FunneledCSSE, FindFirstCSSE}
import puck.graph.immutable.AccessGraph
import puck.graph.{ResultT, JavaNodeKind, JavaSolver}
import puck.graph.io.ConstraintSolvingSearchEngineBuilder
import puck.javaAG.immutable.DeclHolder
import puck.search.SearchEngine

/**
 * Created by lorilan on 12/09/14.
 */
//CSSE : Constraint Solving Search Engine
trait JavaCSSEBuilder
  extends ConstraintSolvingSearchEngineBuilder{
  val violationsKindPriority = JavaSolver.violationPrioritySeq
}

object JavaFindFirstCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "First solution"

  def apply(graph : AccessGraph) : SearchEngine[ResultT] =
    new FindFirstCSSE(violationsKindPriority, graph, JavaSolverBuilder)
}

object JavaFunneledCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Funneled"

  def apply(graph : AccessGraph) : SearchEngine[ResultT] =
    new FunneledCSSE(violationsKindPriority, graph, JavaSolverBuilder)
}

object JavaTryAllCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Try all"

  def apply(graph : AccessGraph) : SearchEngine[ResultT] =
    new TryAllCSSE(violationsKindPriority, graph, JavaSolverBuilder)
}