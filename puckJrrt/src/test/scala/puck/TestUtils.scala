package puck

import puck.graph.DependencyGraph
import puck.jastadd.ExtendJGraphUtils
import puck.search.{SearchEngine, SearchState}
import puck.util.LoggedEither

import scalaz.\/-

/**
  * Created by cedric on 26/04/2016.
  */
object TestUtils {
  def showSuccess(ss : SearchState[(DependencyGraph, Int)]) = {
    val LoggedEither(_, \/-((g, _))) = ss.loggedResult
    QuickFrame(g, "G", ExtendJGraphUtils.dotHelper)
  }
  def showEngineSuccesses(engine : SearchEngine[(DependencyGraph, Int)]) =
    engine.successes foreach showSuccess
}
