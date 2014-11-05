package puck.javaAG.mutable

import puck.graph.mutable.AccessGraph
import puck.graph.mutable.backTrack.Recording
import puck.graph.mutable.io.ConstraintSolvingSearchEngineBuilder
import puck.javaAG.mutable.nodeKind._
import puck.search.SearchEngine

/**
 * Created by lorilan on 12/09/14.
 */
//CSSE : Constraint Solving Search Engine
trait JavaCSSEBuilder
  extends ConstraintSolvingSearchEngineBuilder[JavaNodeKind]{
  val violationsKindPriority = List[JavaNodeKind](Field(), Constructor(), Class(), Interface())
}

/*object JavaFindFirstCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "First solution"

  def apply(graph : AccessGraph[JavaNodeKind]) : SearchEngine[Recording[JavaNodeKind]] =
    new FindFirstCSSE(violationsKindPriority, graph, JavaSolverBuilder)
}

object JavaFunneledCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Funneled"

  def apply(graph : AccessGraph[JavaNodeKind]) : SearchEngine[Recording[JavaNodeKind]] =
    new FunneledCSSE(violationsKindPriority, graph, JavaSolverBuilder)
}

object JavaTryAllCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Try all"

  def apply(graph : AccessGraph[JavaNodeKind]) : SearchEngine[Recording[JavaNodeKind]] =
    new TryAllCSSE(violationsKindPriority, graph, JavaSolverBuilder)
}*/