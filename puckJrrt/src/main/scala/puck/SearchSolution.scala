package puck

import puck.graph.constraints.ConstraintsMaps
import puck.graph.constraints.search.BlindControl
import puck.graph.transformations.Recording
import puck.graph.{DecoratedGraph, DependencyGraph, LoggedSuccess, Metrics, MutabilitySet}
import puck.jastadd.ExtendJGraphUtils._
import puck.search._
import puck.util.{PuckLog, PuckLogger}

/**
  * Created by Loïc Girault on 29/07/16.
  */
object SearchSolution {

  def apply
    ( g : DependencyGraph, cm : ConstraintsMaps, mutabilitySet : MutabilitySet)
    ( implicit logger : PuckLogger)  : Seq[SearchState[DecoratedGraph[_]]] = {
        val f = Metrics.fitness1(_: DependencyGraph, cm, kViols = 1, kComplex = 1).toDouble

        //val strategy = new AStarSearchStrategy[DecoratedGraph[Any]](DecoratedGraphEvaluator.equalityByMapping(f))
        val strategy = new BreadthFirstSearchStrategy[DecoratedGraph[Any]]
        val control = new BlindControl(Rules, g.mileStone, cm, mutabilitySet, violationsKindPriority).
          asInstanceOf[SearchControl[DecoratedGraph[Any]]]

        val engine = new SearchEngine(strategy, control, Some(1))
        logger.writeln("search start !")
        puck.util.Time.time(logger, PuckLog.defaultVerbosity)(engine.explore())

        logger.writeln(engine.successes.size + " solutions found")
        engine.successes   }


   def apply(p : Project, baseName : String)
           (implicit logger : PuckLogger): Unit = {
    val dg2ast = p.loadGraph()

    p.parseConstraints(dg2ast) match {
      case None => logger.writeln("no output constraints")
      case Some(cm) =>
        this.apply(dg2ast.initialGraph, cm, dg2ast.initialMutability) map (st => (st.uuid(), st.loggedResult)) foreach {
          case (id, LoggedSuccess(_, (g,_))) =>
            import puck.util.FileHelper.FileOps
            val recFile = p.workspace \  s"$baseName-solution$id.pck"
            Recording.write(recFile.getAbsolutePath, dg2ast.nodesByName, g)
        }
    }
  }
}
