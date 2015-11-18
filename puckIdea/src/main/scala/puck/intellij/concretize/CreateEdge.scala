package puck.intellij.concretize

import com.intellij.openapi.module.Module
import com.intellij.psi.PsiElementFactory
import puck.graph._
import puck.graph.transformations.{Edge, Regular, Transformation}
import puck.intellij.{TypedKindDeclHolder, PackageDummyWrapper, PsiNodeWrapper}
import puck.util.PuckLogger

/**
 * Created by lorilan on 04/11/15.
 */
object CreateEdge {

  def unapply(t : Transformation) : Option[DGEdge] =
    t match {
      case Transformation(Regular, Edge(e)) => Some(e)
      case _ => None
    }

  def apply
  ( resultGraph : DependencyGraph,
    id2declMap: Map[NodeId, PsiNodeWrapper],
    edge : DGEdge)
  (implicit factory : PsiElementFactory,
     module: Module,
     logger : PuckLogger) : Unit =
    (edge.kind, id2declMap(edge.source), id2declMap(edge.target)) match {
    case (Contains, PackageDummyWrapper, tdh : TypedKindDeclHolder ) =>

      val clss = tdh.node
      clss.setName(resultGraph.fullName(edge.source)+ "." + clss.getName)
      //val d = FilenameIndex.getVirtualFilesByName(module.getProject, node.name, module.getModuleContentScope)

    case _ => ()
  }

}
