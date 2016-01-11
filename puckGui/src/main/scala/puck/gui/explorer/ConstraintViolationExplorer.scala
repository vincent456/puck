package puck
package gui
package explorer

import java.awt.event.{ActionEvent, MouseEvent, MouseAdapter}
import javax.swing.event.TreeModelListener
import javax.swing.{AbstractAction, JPopupMenu, JTree}

import puck.graph._
import javax.swing.tree._


import scala.swing.{Publisher, Swing, ScrollPane}


class CCTree(g : DependencyGraph, parentToChildMap : Map[DGEdge, List[DGEdge]])
  extends JTree(new CCTreeModel(parentToChildMap)){


  lazy val topLevelEdges = parentToChildMap(CCTreeModel.root)

  implicit def idToNameString(dg : DependencyGraph, nid : NodeId) : String =
    dg.getNode(nid) match {
      case n : ConcreteNode =>
        val name =
          if(n.kind.kindType == ValueDef)
            dg.container(n.id) map {
              idToNameString(dg, _)
            } getOrElse "OrphanDefinition"
          else n.name

        name + ShowDG.typeHolderCord(dg, dg.styp(n.id))
      case vn : VirtualNode => vn.name(dg)
    }

  def edgeToString(nodeName : NodeId => String, e:  DGEdge ) : String =
    if(e.source == e.target) s"${g.getNode(e.source).kind} - ${nodeName(e.source)}"
    else {
      val (ksrc, ktgt) = (g.getNode(e.source).kind, g.getNode(e.target).kind)
      e.kind match {
        case AbstractEdgeKind =>
          val s = if(ksrc.toString endsWith "s") "es"
          else "s"
          if (ksrc == ktgt) s"$ksrc$s : ${nodeName(e.source)} -> ${nodeName(e.target)}"
          else s"$ksrc : ${nodeName(e.source)} -> $ktgt : ${nodeName(e.target)}"

        case _ =>
          s"${nodeName(e.source)} ($ksrc) - ${e.kind} -> ${nodeName(e.target)} ($ktgt)"
      }
    }

  def numViolations(acc : Map[DGEdge, Int], e : DGEdge) : Map[DGEdge, Int] = {
    val children = parentToChildMap get e

    if( children.isEmpty) acc + (e -> 1)
    else {
      val acc1 = children.get.foldLeft(acc)(numViolations)
      val childrenViolations : List[Int] = children.get map acc1.apply
      acc1 + (e -> childrenViolations.sum)
    }

  }

  lazy val numViolationsMap = numViolations(Map(), CCTreeModel.root)

  override def convertValueToText
  (value: AnyRef, selected: Boolean,
   expanded: Boolean, leaf: Boolean,
   row: Int, hasFocus: Boolean) : String = {

    value match {
      case violation: DGEdge =>

      val txt =
        if(violation == CCTreeModel.root) ""
        else {
          val f: NodeId => String =
            if (topLevelEdges contains violation) g.fullName
            else idToNameString(g, _)
          edgeToString(f, violation)
        }

        val numVs = numViolationsMap(violation)
        if(leaf) txt
        else txt + s" ($numVs violation(s))"
      case _ => ""
    }

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


object ConstraintViolationExplorer {
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
  (graph : DependencyGraph,
   ks : Seq[KindType])
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
            createChildToParentMap(graph, remainings)(addChildToParentLink(m, e, pe), pe)

          case (None, Some(ptarget)) =>
            val pe = new DGEdge(AbstractEdgeKind, src, ptarget)
            createChildToParentMap(graph, remainings)(addChildToParentLink(m, e, pe), pe)
          case (Some(psource), None) =>
            val pe = new DGEdge(AbstractEdgeKind, psource, tgt)
            createChildToParentMap(graph, remainings)(addChildToParentLink(m, e, pe), pe)
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

import ConstraintViolationExplorer._
class ConstraintViolationExplorer
(publisher : Publisher,
 violations : Seq[DGEdge])
(implicit graph : DependencyGraph,
 graphUtils : GraphUtils)
  extends ScrollPane {

    //val swingService : Swing

    val kindSeq = Seq(TypeDecl, NameSpace)

    val childToParentMap = violations.foldLeft(Map[DGEdge, Option[DGEdge]]())(createChildToParentMap(graph, kindSeq))

    val tree = new CCTree(graph, createParentToChildMap(childToParentMap))
    //tree.setCellRenderer(new CCTreeCellRenderer(graph))
    contents = swing.Component.wrap(tree)

    tree.addMouseListener( new MouseAdapter {

      override def mouseClicked(e : MouseEvent) : Unit =  {
        val path : TreePath = tree.getPathForLocation(e.getX, e.getY)

        if(path!= null){
          path.getLastPathComponent match {
            case edge : DGEdge =>
              if(isRightClick(e)){
                val menu : JPopupMenu = new ViolationMenu(publisher, edge){
                  add( new AbstractAction("Focus in graph explorer") {
                    def actionPerformed(e: ActionEvent): Unit =
                      publisher.publish(GraphFocus(graph, edge))
                  })
                }
                Swing.onEDT(menu.show(ConstraintViolationExplorer.this.peer, e.getX, e.getY))
              }
            case _ => ()
          }
        }
      }
    })



}
