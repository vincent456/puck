package puck.intellij.concretize

import java.io.File
import java.util

import com.intellij.openapi.module.Module
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiElementFactory, JavaPsiFacade}
import com.intellij.psi.search.FilenameIndex
import puck.graph._
import puck.graph.transformations.{CNode, Regular, Transformation}
import puck.intellij._
import puck.javaGraph.nodeKind._
import puck.util.PuckLogger

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
  (implicit factory : PsiElementFactory,
   module: Module,
   logger : PuckLogger) : Map[NodeId, PsiNodeWrapper] =
    node.kind match {
      case Class =>
        id2declMap + (node.id ->
          ClassDeclHolder(factory.createClass(node.name)))

      case Interface =>
        id2declMap + (node.id ->
          InterfaceDeclHolder(factory.createInterface(node.name)))

      case Constructor =>
        id2declMap + (node.id ->
          ConstructorDeclHolder(factory.createConstructor()))

      case Package =>
        val path = resultGraph.containerPath(node.id) map resultGraph.getConcreteNode
        if(path.head.id != DependencyGraph.rootId)
          error("need fully attached node")
        createPackageDirectories(path.tail)
        id2declMap + (node.id -> PackageDummyWrapper)

      case Definition =>
        id2declMap + (node.id -> EmptyDef)
      case _ =>
        println(s"creation of $node ignored")
        id2declMap
    }


  def createPackageDirectories(path : Seq[ConcreteNode]) : Option[VirtualFile] =
    PuckToolWindow.generatedSourcesRoot map {
      root =>
        path.foldLeft(root) {
          (parent, n) =>
            parent.findChild(n.name) match {
              case null => parent.createChildDirectory(this, n.name)
              case c => c
            }
        }
    }

  def getPackageDirectories(path : Seq[ConcreteNode])
                           (implicit module: Module): Seq[VirtualFile] = {


    def aux(vfs : Seq[VirtualFile],
            path : Seq[ConcreteNode]) : Seq[VirtualFile] = {
      val candidates = vfs filter (_.getName == path.head.name)

      (candidates, path) match {
        case (Seq(), _) => Seq()
        case (_, Seq(last)) => candidates
        case (results, current +: nexts) =>
          aux(results flatMap (_.getChildren.toSeq), nexts)
        case _ => error("should not happen")
      }
    }

    val roots = ModuleRootManager.getInstance(module).getSourceRoots()

    aux(roots.toSeq flatMap (_.getChildren.toSeq), path)
  }





}

