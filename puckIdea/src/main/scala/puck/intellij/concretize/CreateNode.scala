package puck.intellij.concretize

import com.intellij.openapi.module.Module
import com.intellij.psi.{PsiElementFactory, JavaPsiFacade}
import com.intellij.psi.search.FilenameIndex
import puck.graph._
import puck.graph.transformations.{CNode, Regular, Transformation}
import puck.intellij.{PackageDummyWrapper, InterfaceDeclHolder, ClassDeclHolder, PsiNodeWrapper}
import puck.javaGraph.nodeKind._

/**
 * Created by lorilan on 03/11/15.
 */
object CreateNode {

  def unapply(t : Transformation) : Option[ConcreteNode] =
    t match {
      case Transformation(Regular, CNode(n)) => Some(n)
      case _ => None
    }

  def apply
  (resultGraph : DependencyGraph,
   id2declMap: Map[NodeId, PsiNodeWrapper],
    node : ConcreteNode)
  (implicit factory : PsiElementFactory, module: Module) : Map[NodeId, PsiNodeWrapper] =
    node.kind match {
      case Class =>
        id2declMap + (node.id ->
          ClassDeclHolder(factory.createClass(node.name)))

      case Interface =>
        id2declMap + (node.id ->
          InterfaceDeclHolder(factory.createInterface(node.name)))

      case Package =>
        val d = FilenameIndex.getVirtualFilesByName(module.getProject, node.name, module.getModuleContentScope)
        id2declMap + (node.id -> PackageDummyWrapper)
    }




}

