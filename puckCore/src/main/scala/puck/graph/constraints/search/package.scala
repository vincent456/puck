package puck.graph.constraints

import puck.graph._
import puck.graph.transformations.TransformationRules
import puck.search.{SearchControl, SearchStrategy}

/**
  * Created by mikal on 04/05/2016.
  */
package object search {
  type ControleBuilder = (TransformationRules, DependencyGraph, ConstraintsMaps, ConcreteNode) => SearchControl[SResult]
  type StrategyBuilder[T] = () => SearchStrategy[T]

  type AutomataState = Int


}
