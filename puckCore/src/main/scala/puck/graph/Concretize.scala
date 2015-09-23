package puck.graph

import puck.graph.transformations.TransformationRules
import puck.util.LoggedEither._

import scalaz.std.set._

object Concretize {

  def apply(g : DependencyGraph, rules : TransformationRules) : LoggedTG =
    if(g.virtualNodes.isEmpty) LoggedSuccess(g)
    else {
      val vn = g.virtualNodes.head

      g.content(vn.id).foldLoggedEither(g){ (g0, cid) =>
        val usedElts : Set[NodeId] = Metrics.outgoingDependencies(g0, cid).map(_._2)
        val c = g0.getConcreteNode(cid)
        val potentialContainers = g0.concreteNodes.filter(g0.canContain(_, c))
        val pcWithCohesion = potentialContainers.foldLeft(List[(Double, NodeId)]()){ (l, pc) =>
          (Metrics.relativeCohesion(g0, usedElts, pc.id), pc.id) :: l
        }

        val best = pcWithCohesion.sortBy(_._1).head._2
        c.kind.kindType match {
          case TypeDecl => rules.move.staticDecl(g, cid, best)
          case _ => ???
        }

      } flatMap {
        g =>
        apply(g removeVirtualNode vn, rules)
      }

     }

}
