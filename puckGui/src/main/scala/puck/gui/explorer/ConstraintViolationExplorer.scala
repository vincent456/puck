package puck
package gui.explorer

import java.awt.{Component, BorderLayout}
import javax.swing.{JLabel, JPanel, JTree}

import puck.graph._
import javax.swing.tree.{TreeNode, TreePath, TreeCellRenderer, DefaultMutableTreeNode}


import scala.swing.ScrollPane


class CCTree(g : DependencyGraph, root : TreeNode)
  extends JTree(root, false){
  override def convertValueToText
  (value: AnyRef, selected: Boolean,
   expanded: Boolean, leaf: Boolean,
   row: Int, hasFocus: Boolean) : String =
    value match {
      case null => ""
      case node : DefaultMutableTreeNode if node.getUserObject != null =>

        val violation = node.getUserObject.asInstanceOf[DGEdge]
        import ShowDG._
        (g, violation).shows
      case _ => ""
  }
}

class ConstraintViolationExplorer
( graph : DependencyGraph,
  violations : Seq[DGEdge])
extends ScrollPane {


    val kindSeq = Seq(TypeDecl, NameSpace)

    val sortedViolations = violations.foldLeft(Map[DGEdge, Option[DGEdge]]())(createHierarchy(kindSeq))

    val roots = buildTree(Seq(), Map(), sortedViolations.toSeq)

    val root = new DefaultMutableTreeNode()

    roots foreach root.add

    val tree = new CCTree(graph, root)
    //tree.setCellRenderer(new CCTreeCellRenderer(graph))
    contents = swing.Component.wrap(tree)


  def mapWithE(m : Map[DGEdge, Option[DGEdge]], e : DGEdge) =
    if(m contains e) m
    else m + (e -> None)

  def createHierarchy
    (ks : Seq[KindType])
    (m : Map[DGEdge, Option[DGEdge]], e : DGEdge
     ) : Map[DGEdge, Option[DGEdge]] = ks match {
    case  Nil => mapWithE(m,e)
    case k +: remainings =>
      val src = e.source
      val tgt = e.target
      val spsource = graph.containerOfKindType(k, e.source)
      val sptarget = graph.containerOfKindType(k, e.target)
      (spsource, sptarget) match {
        case (None, None)
        | (Some(`src`), Some(`tgt`)) => mapWithE(m,e)

        case (Some(psource), Some(ptarget)) =>

          val pe = new DGEdge(AbstractEdgeKind, psource, ptarget)
          val m1 = m getOrElse (e, None) match {
               case None => m + ( e -> Some(pe))
               case Some(storedParent) =>
                 if(storedParent == pe) m
                 else error()
            }

          createHierarchy(remainings)(m1, pe)
        case _ => error()
      }
    }


  def buildTree(roots: Seq[DefaultMutableTreeNode],
                m : Map[DGEdge, DefaultMutableTreeNode],
                s : Seq[(DGEdge,Option[DGEdge])]) : Seq[DefaultMutableTreeNode] = s match {
    case Nil => roots
    case (e, Some(pe)) +: tl =>
      val tne = m getOrElse (e, new DefaultMutableTreeNode(e))
      val tnpe = m getOrElse (pe, new DefaultMutableTreeNode(pe))

      tnpe add tne

      buildTree(roots, m + (e -> tne) + (pe -> tnpe), tl)

    case (e, None) +: tl =>
      val tne = m getOrElse (e, new DefaultMutableTreeNode(e))

      buildTree(tne +: roots, m + (e -> tne), tl)
  }



}
