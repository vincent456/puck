package puck.intellij.concretize

import com.intellij.psi.PsiJavaFile
import puck.graph.{DependencyGraph, NodeId, DGEdge}
import puck.graph.transformations._
import puck.intellij.{TypedKindDeclHolder, PackageDummyWrapper, PsiNodeWrapper}
import puck.javaGraph.nodeKind._

/**
 * Created by lorilan on 04/11/15.
 */
object Move {

  def unapply(t : Transformation) : Option[(DGEdge, NodeId)] =
    t match {
      case Transformation(_, RedirectionOp(e, Source(newSource))) =>
        Some((e, newSource))
      case _ => None
    }



  def apply
  ( reenactor : DependencyGraph,
    id2declMap: Map[NodeId, PsiNodeWrapper],
    e : DGEdge,
    n : NodeId) : Unit = {
    val container = reenactor getConcreteNode e.container
    val contained  = reenactor getConcreteNode e.content
    val newContainer = reenactor getConcreteNode n

    assert (container.kind == newContainer.kind)

    (id2declMap(e.container), id2declMap(e.content)) match {
      case (PackageDummyWrapper, t : TypedKindDeclHolder)=>
        val c = t.node
        val f = c.getContainingFile.asInstanceOf[PsiJavaFile]
        //todo check if return also innerclasses
        if(f.getClasses.length==1){
          f.setPackageName(newContainer.name)
          //f.getVirtualFile.move()
        }

    }


  }
}
