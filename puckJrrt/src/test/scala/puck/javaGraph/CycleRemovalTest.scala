package puck.javaGraph

import puck.graph._
import puck.util.{GenericGraphAlgorithms, WeightedDirectedGraph, GreedyCycleRemover}
import puck.util.GreedyCycleRemover.WDGHelper
import puck.{QuickFrame, Settings, AcceptanceSpec}

/**
  * Created by lorilan on 08/12/15.
  */
class CycleRemovalTest
  extends AcceptanceSpec {


  val examplesPath = Settings.testExamplesPath + "/cycle"
  feature("Layering algorithm"){
    val rootDir = examplesPath + "/3packages3classes"
    scenario("3 package 3 classes"){
      val scenar = ScenarioFactory.fromDirectory(rootDir)
      import scenar.graph


      val isRelevantNode : (DependencyGraph, NodeId) => Boolean = {
        (g, id) =>
          val n = g.getNode(id)
          n.kind.kindType match {
            case NameSpace | TypeDecl => true
            case _ => false
          }
      }


//      val edges = reduceGraph(graph, isRelevantNode)
//      //graph.usesList ++ graph.typeUsesList
//      val nodes = graph.nodesId.toSet filter (isRelevantNode(graph, _))
//      val remover = new GreedyCycleRemover(NaiveHelper)
//      val edgesToRemove = remover.edgesToRemove((nodes, edges))

      val rg = GenericGraphAlgorithms.reduceGraphG(graph, isRelevantNode)
      QuickFrame(rg, "RG",JavaDotHelper)
//      val edgesToRemove = greedyCycleRemoval(rg)

      val wdg = WeightedDirectedGraph.fromDG(graph, isRelevantNode)
      val remover = new GreedyCycleRemover(WDGHelper)
      val edgesToRemove = remover.edgesToRemove(wdg).toList

 //      println(edges.mkString(","))
//      println(nodes.mkString(","))
      println(edgesToRemove)
      assert(edgesToRemove.nonEmpty)
    }

  }

}
