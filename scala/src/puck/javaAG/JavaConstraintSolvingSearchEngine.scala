package puck.javaAG

import puck.graph.constraints.{CSInitialSearchState, ConstraintSolvingChoices, ConstraintSolvingSearchEngine}
import puck.graph.{AGNode, AccessGraph}
import puck.javaAG.nodeKind._
import puck.search.SearchState
import puck.util.Logger

/**
 * Created by lorilan on 02/07/14.
 */
class JavaConstraintSolvingSearchEngine(val graph : AccessGraph[JavaNodeKind],
                                        val logger : Logger[Int],
                                        val printTrace : SearchState[ConstraintSolvingChoices[JavaNodeKind],
                                          Option[AGNode[JavaNodeKind]]] => Unit)
  extends ConstraintSolvingSearchEngine[JavaNodeKind] {

  val initialState = new CSInitialSearchState(this,
    new JavaSolver(graph, this), printTrace)

  val violationsKindPriority = List[JavaNodeKind](Field(), Constructor(), Class(), Interface())

  override def explore(){
    graph.transformations.startRegister()
    super.explore()
  }
}
