package puck.control

import puck.graph.{DependencyGraph, Recording}

import scala.swing.{Component, Publisher}

/**
  * Created by Loïc Girault on 12/8/16.
  */
trait HistoryHandler {

  val bus : Publisher

  def fireUpdate() : Unit =
    bus publish GraphUpdate(graph)

  def firePushEvent(previousHead : DependencyGraph) : Unit =
    bus publish Pushed(graph, previousHead)

  def firePopEvent(poppedGraph : DependencyGraph) : Unit =
    bus publish Popped(poppedGraph, graph)

  def graph : DependencyGraph

  def setInitialGraph(g: DependencyGraph): Unit

  def pushGraph(graph: DependencyGraph) : Unit

  def rewriteHistory(rec: Recording): Unit

  def load(rec: Recording): Unit

  def view() : Component
}

