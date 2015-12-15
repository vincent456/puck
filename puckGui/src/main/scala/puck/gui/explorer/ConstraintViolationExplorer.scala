package puck
package gui.explorer

import javax.swing.{JScrollPane, JTree}

import puck.graph._
import javax.swing.tree.DefaultMutableTreeNode

import puck.gui.{CCUpdate, DGUpdate}

import scala.swing.{FlowPanel, ScrollPane}



class ConstraintViolationExplorer
( graph : DependencyGraph,
  violations : Seq[DGEdge])
extends ScrollPane {


    println(s"${violations.size} violations")
    val kindSeq = Seq(TypeDecl, NameSpace)

    val sortedViolations = violations.foldLeft(Map[DGEdge, Option[DGEdge]]())(createHierarchy(kindSeq))

    println(s"${sortedViolations.size} sorted violations")

    val roots = buildTree(Seq(), Map(), sortedViolations.toSeq)

    val root = new DefaultMutableTreeNode()

    println(s"${roots.size} roots")
    roots foreach root.add

    val tree = new JTree(root)

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

          val pe = Uses(psource, ptarget)
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

      println(s"$e in $pe")

      buildTree(roots, m + (e -> tne) + (pe -> tnpe), tl)

    case (e, None) +: tl =>
      val tne = m getOrElse (e, new DefaultMutableTreeNode(e))

      println(s"root : $e")
      buildTree(tne +: roots, m + (e -> tne), tl)
  }



}
