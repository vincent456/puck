package puck.javaGraph

import puck.graph.constraints.search.{TryAllCSSE, FunneledCSSE, FindFirstCSSE}
import puck.graph._
import puck.graph.io.ConstraintSolvingSearchEngineBuilder
import puck.graph.transformations.Transformation
import puck.search.SearchEngine

//CSSE : Constraint Solving Search Engine
trait JavaCSSEBuilder
  extends ConstraintSolvingSearchEngineBuilder{
  val violationsKindPriority = JavaViolationPrioritySeq
}

object JavaFindFirstCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "First solution"

  def apply(initialRecord : Seq[Transformation], graph : DependencyGraph,
            automaticConstraintLoosening : Boolean) : SearchEngine[ResultT] =
    new FindFirstCSSE(violationsKindPriority, graph, JavaSolverBuilder, automaticConstraintLoosening)
}

object JavaFunneledCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Funneled"

  def apply(initialRecord : Seq[Transformation], graph : DependencyGraph,
            automaticConstraintLoosening : Boolean) : SearchEngine[ResultT] =
    new FunneledCSSE(initialRecord, violationsKindPriority, graph, JavaSolverBuilder, automaticConstraintLoosening)
}

object JavaTryAllCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Try all"

  def apply(initialRecord : Seq[Transformation], graph : DependencyGraph,
            automaticConstraintLoosening : Boolean) : SearchEngine[ResultT] =
    new TryAllCSSE(violationsKindPriority, graph, JavaSolverBuilder, automaticConstraintLoosening)
}