package puck.graph

import puck.graph.transformations.TransformationRules
import puck.util.LoggedEither._
import scalaz._, Scalaz._

object Concretize {

  def apply(g : DependencyGraph, rules : TransformationRules) : LoggedTG =
    if(g.virtualNodes.isEmpty) LoggedSuccess(g)
    else {
      val vn = g.virtualNodes.head

      g.content(vn.id).foldLoggedEither(g){ (g0, cid) =>
        val usedElts : Set[NodeId] = Metrics.outgoingDependencies(cid, g0).map(_.used)
        val c = g0.getConcreteNode(cid)
        val potentialContainers = g0.concreteNodes.filter(g0.canContain(_, c))
        val pcWithCohesion = potentialContainers.foldLeft(List[(Double, NodeId)]()){ (l, pc) =>
          (Metrics.relativeCohesion(usedElts, pc.id, g0), pc.id) :: l
        }

        val best = pcWithCohesion.sortBy(_._1).head._2
        g.kindType(c) match {
          case TypeDecl => rules.move.typeDecl(g, cid, best)
          case _ => ???
        }

      } flatMap {
        g =>
        apply(g removeVirtualNode vn, rules)
      }

     }

}
