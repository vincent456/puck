package puck.javaGraph

import puck.graph.constraints.Solver
import puck.graph.constraints.search._
import puck.graph._
import puck.graph.io.ConstraintSolvingSearchEngineBuilder
import puck.graph.transformations.Transformation
import puck.search.{FunneledSeachEngine, FindFirstSearchEngine, TryAllSearchEngine, SearchEngine}
import SearchEngine.InitialStateFactory
//CSSE : Constraint Solving Search Engine
trait JavaCSSEBuilder
  extends ConstraintSolvingSearchEngineBuilder{
  val violationsKindPriority = JavaViolationPrioritySeq

  def buildSearchEngine
  ( initialRecord : Seq[Transformation],
    initialStateFactory : InitialStateFactory[ResultT]
   ) : SearchEngine[ResultT]


  def apply(initialRecord : Seq[Transformation], graph : DependencyGraph,
            automaticConstraintLoosening : Boolean) : SearchEngine[ResultT] = {

    val dm = new ConstraintSolvingSearchEngineDecisionMaker(JavaViolationPrioritySeq)

    val solver = new Solver(dm, JavaTransformationRules, automaticConstraintLoosening)

    val searchEngine = buildSearchEngine(initialRecord,
      k => new CSInitialSearchState(solver, graph, k))

    dm.searchEngine = searchEngine

    searchEngine
  }
}

object JavaFindFirstCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "First solution"

  def buildSearchEngine
  ( initialRecord : Seq[Transformation],
    initialStateFactory : InitialStateFactory[ResultT]) =
    new FindFirstSearchEngine[ResultT](initialStateFactory)

}

object JavaFunneledCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Funneled"

  def buildSearchEngine
  ( initialRecord : Seq[Transformation],
    initialStateFactory : InitialStateFactory[ResultT]
    ) : SearchEngine[ResultT] =
    new FunneledSeachEngine[ResultT](initialStateFactory,
      new ConstraintSolvingStateEvaluator(initialRecord))
}

object JavaTryAllCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Try all"

  def buildSearchEngine
  ( initialRecord : Seq[Transformation],
    initialStateFactory : InitialStateFactory[ResultT]
    ) : SearchEngine[ResultT] =
    new TryAllSearchEngine[ResultT](initialStateFactory)
}