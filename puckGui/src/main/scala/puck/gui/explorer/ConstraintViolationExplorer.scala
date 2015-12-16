package puck
package gui.explorer

import java.awt.{Component, BorderLayout}
import javax.swing.event.TreeModelListener
import javax.swing.{JLabel, JPanel, JTree}

import puck.graph._
import javax.swing.tree._


import scala.swing.ScrollPane


class CCTree(g : DependencyGraph, parentToChildMap : Map[DGEdge, List[DGEdge]])
  extends JTree(new CCTreeModel(parentToChildMap)){

  import ShowDG._

  override def convertValueToText
  (value: AnyRef, selected: Boolean,
   expanded: Boolean, leaf: Boolean,
   row: Int, hasFocus: Boolean) : String =
    value match {
      case CCTreeModel.root => ""
      case violation : DGEdge =>
        (g, violation).shows
      case _ => ""
  }
}


object CCTreeModel {
  val root : DGEdge = new DGEdge(AbstractEdgeKind, 0, 0)
}
class CCTreeModel
( parentToChildMap : Map[DGEdge, List[DGEdge]]
  ) extends TreeModel {

  private def getChildren(parent: scala.Any) : List[DGEdge] =
    parent match {
      case e : DGEdge =>
        parentToChildMap(e)
      case _ => error(parent.getClass+ " where DGEdge was expected")
    }

  def getChild(parent: scala.Any, index: NodeId): DGEdge =
    getChildren(parent)(index)

  def isLeaf(node: scala.Any): Boolean =
    node match {
      case e : DGEdge => parentToChildMap.get(e).isEmpty
      case _ => error(node.getClass+ " where DGEdge was expected")
    }

  def getChildCount(parent: scala.Any): NodeId =
    getChildren(parent).size

  def getRoot: AnyRef = CCTreeModel.root

  def getIndexOfChild(parent: scala.Any, child: scala.Any): NodeId =
    getChildren(parent).indexOf(child)

  def valueForPathChanged(path: TreePath, newValue: scala.Any): Unit = ()
  def removeTreeModelListener(l: TreeModelListener): Unit = ()
  def addTreeModelListener(l: TreeModelListener): Unit = ()

}


class ConstraintViolationExplorer
( graph : DependencyGraph,
  violations : Seq[DGEdge])
  extends ScrollPane {


    val kindSeq = Seq(TypeDecl, NameSpace)

    val childToParentMap = violations.foldLeft(Map[DGEdge, Option[DGEdge]]())(createChildToParentMap(kindSeq))

    val tree = new CCTree(graph, createParentToChildMap(childToParentMap))
    //tree.setCellRenderer(new CCTreeCellRenderer(graph))
    contents = swing.Component.wrap(tree)


  def mapWithE(m : Map[DGEdge, Option[DGEdge]], e : DGEdge) =
    if(m contains e) m
    else m + (e -> None)

  def addChildToParentLink
  (m : Map[DGEdge, Option[DGEdge]],
   e : DGEdge, pe : DGEdge) : Map[DGEdge, Option[DGEdge]] = {
    m getOrElse(e, None) match {
      case None => m + (e -> Some(pe))
      case Some(storedParent) =>
        if (storedParent == pe) m
        else error(s"storedParent = $storedParent, pe = $pe")
    }
  }

  def createChildToParentMap
    (ks : Seq[KindType])
    (m : Map[DGEdge, Option[DGEdge]], e : DGEdge
     ) : Map[DGEdge, Option[DGEdge]] = {
    ks match {
      case Nil => mapWithE(m, e)
      case k +: remainings =>
        val src = e.source
        val tgt = e.target
        val spsource = graph.containerOfKindType(k, e.source)
        val sptarget = graph.containerOfKindType(k, e.target)
        (spsource, sptarget) match {
          case (None, None)
               | (Some(`src`), Some(`tgt`)) => mapWithE(m, e)

          case (Some(psource), Some(ptarget)) =>
            val pe = new DGEdge(AbstractEdgeKind, psource, ptarget)
            createChildToParentMap(remainings)(addChildToParentLink(m, e, pe), pe)

          case (None, Some(ptarget)) =>
            val pe = new DGEdge(AbstractEdgeKind, src, ptarget)
            createChildToParentMap(remainings)(addChildToParentLink(m, e, pe), pe)
          case (Some(psource), None) =>
            val pe = new DGEdge(AbstractEdgeKind, psource, tgt)
            createChildToParentMap(remainings)(addChildToParentLink(m, e, pe), pe)
        }
    }
  }

  def createParentToChildMap
  ( childToParentMap : Map[DGEdge, Option[DGEdge]] ) : Map[DGEdge, List[DGEdge]] = {
    def add(m : Map[DGEdge, List[DGEdge]], pe: DGEdge, e: DGEdge) : Map[DGEdge, List[DGEdge]] = {
      val l = m.getOrElse(pe, List())
      m + (pe -> ( e :: l))
    }


    childToParentMap.foldLeft(Map[DGEdge, List[DGEdge]]()){
      case (m, (e, None)) => add(m, CCTreeModel.root, e)
      case (m, (e, Some(pe))) => add(m, pe, e)
    }
  }


}
