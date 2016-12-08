package puck
import puck.graph.{DependencyGraph, Recording}

import scala.swing.Component

/**
  * Created by Loïc Girault on 12/8/16.
  */
trait HistoryHandler {

  def graph : DependencyGraph

  def setInitialGraph(g: DependencyGraph): Unit

  def pushGraph(graph: DependencyGraph)

  def rewriteHistory(rec: Recording): Unit

  def load(rec: Recording): Unit

  def view() : Component
}

