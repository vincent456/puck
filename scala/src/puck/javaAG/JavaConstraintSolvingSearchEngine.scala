package puck.javaAG

import puck.graph.constraints.{GradedConstraintSolvingSearchEngine, CSInitialSearchState, ConstraintSolvingChoices}
import puck.graph.{AGNode, AccessGraph}
import puck.javaAG.nodeKind._
import puck.search.SearchState
import puck.util.Logger

/**
 * Created by lorilan on 02/07/14.
 */
class JavaConstraintSolvingSearchEngine(val graph : AccessGraph[JavaNodeKind],
                                        searchEnginelogger : Logger[Int],
                                        solverLogger : Logger[Int],
                                        val printTrace : SearchState[ConstraintSolvingChoices[JavaNodeKind],
                                          Option[AGNode[JavaNodeKind]]] => Unit)
  extends GradedConstraintSolvingSearchEngine[JavaNodeKind] {

  val logger = searchEnginelogger

  val initialState = new CSInitialSearchState(this,
    new JavaSolver(graph, solverLogger, this), printTrace)

  val violationsKindPriority = List[JavaNodeKind](Field(), Constructor(), Class(), Interface())

  def grade(st : SearchState[ConstraintSolvingChoices[JavaNodeKind], Option[AGNode[JavaNodeKind]]]) =
    (st.internal.recording.graph.coupling * 1000 ).toInt

  override def search() = {
    graph.transformations.startRegister()
    super.search()
  }
}
