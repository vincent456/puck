package puck.javaAG

import puck.graph.constraints.{CSInitialSearchState, ConstraintSolvingChoices, ConstraintSolvingSearchEngine}
import puck.graph.{AGNode, AccessGraph, NodeKind}
import puck.javaAG.JavaNodeKind.{Interface, Class, Constructor, Field}
import puck.search.SearchState
import puck.util.Logger

import scala.collection.mutable

/**
 * Created by lorilan on 02/07/14.
 */
class JavaConstraintSolvingSearchEngine(val graph : AccessGraph,
                                        val logger : Logger,
                                        val printTrace : SearchState[ConstraintSolvingChoices, Option[AGNode]] => Unit)
  extends ConstraintSolvingSearchEngine {

  val initialState = new CSInitialSearchState(this,
    new JavaSolver(graph, this), printTrace)

  val violationsKindPriority = List[NodeKind](Field(), Constructor(),
    Class(), Interface())

  override def explore(){
    graph.transformations.startRegister()
    super.explore()
  }
}
