package puck.graph

import puck.graph.transformations.TransformationRules
import puck.util.Collections.traverse

import scala.annotation.tailrec
import scalaz._
/**
 * Created by lorilan on 3/16/15.
 */
object Concretize {

  @tailrec
  def apply(g : DependencyGraph, rules : TransformationRules) : Try[DependencyGraph] = {
    if(g.virtualNodes.isEmpty) \/-(g)
    else {
      val vn = g.virtualNodes.head

      val tg = traverse(g.content(vn.id), g){ (g0, cid) =>
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

      }

      //tg.flatMap{g => apply(g.removeVirtualNode(vn.id), rules)}
      tg match {
        case f @ -\/(_) => f
        case \/-(gfinal) => apply(gfinal.removeVirtualNode(vn.id), rules)
      }


    }

  }
}
