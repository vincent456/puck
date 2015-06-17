package puck.graph
package io


import java.io.BufferedWriter

import puck.graph.DGEdge.{UsesK, IsaK}

import scala.collection.mutable

/**
 * Created by lorilan on 13/08/14.
 */
object DotPrinter {

  val redColor = "red"
  class Style(val line: String, val arrowHead: String)

  val isaStyle = new Style("dashed", "empty")
  val containsStyle = new Style("dashed", "open")
  val usesStyle = new Style("bold", "normal")

  val virtualUse = new Style("dotted", "dot")

  class ColorThickness(val color: String, val thickness: Int)

  object ColorThickness {
    val regular = new ColorThickness("black", 2)
    val violation = new ColorThickness(redColor, 5)

    val dominant = new ColorThickness("blue", 2)
    val dominated = new ColorThickness("green", 2)
    val selected = new ColorThickness("black", 5)
  }

}
import VisibilitySet._
case class PrintingOptions
( visibility : VisibilitySet.T,
  printId : Boolean = false,
  printSignatures : Boolean = false,
  selectedUse : Option[DGUses] = None,
  printVirtualEdges : Boolean = false,
  printConcreteUsesPerVirtualEdges : Boolean = true,
  redOnly: Boolean = false)

import DotPrinter._
class DotPrinter
( writer: BufferedWriter,
  graph : DependencyGraph,
  helper : DotHelper,
  printingOptions: PrintingOptions){

  import printingOptions._

  val visibility = printingOptions.visibility
    .setVisibility(DependencyGraph.rootId, Hidden)

  implicit val g = graph

  val idString : Int => String =
    if(printId) id => " (" + id + ")"
    else _ => ""

  private val emptyMap = mutable.Map[NodeId, Boolean]()
  private def isEmpty(id : NodeId) : Boolean = {
    emptyMap get id match {
      case None =>
        val children = g.subTree(id, includeRoot = false)
        val b = children.isEmpty || children.forall(visibility.isHidden)
        emptyMap.put(id, b)
        b
      case Some(b) => b
    }
  }

  def html_safe(str : String) : String = str.replaceAllLiterally(">", "&gt;").replaceAllLiterally("<", "&lt;")

  val signatureString : Option[Type] => String =
    if (printSignatures)
      styp => {
        import ShowDG._
        showDG[Option[Type]](graph).shows(styp).replaceAllLiterally(">", "&gt;") + " "
      }
    else _ => ""


  def writeln(str:String) : Unit = {
    writer write str
    writer newLine()
  }

  private val concreteViolations : Seq[DGEdge] = graph.violations()


/*
 * dot -Tpng give a wrong drawing of the graph when mixing nodes and arcs
 * in the dot file. We stock the arcs and print them separately at the end
 */
  val arcs = scala.collection.mutable.Buffer[String]()

  type EdgeP = (NodeId, NodeId)

  def printArc
  ( status : ColorThickness,
    style : Style,
    slabel : Option[String] = None): EdgeP => Unit = {
    case edge @ (source, target) =>
    def dotId(nid: NodeId) : String = {
      val n = graph.getNode(nid)
      if (helper isDotSubgraph n)
        nodeSubGraphId(n.id.toString)
      else {
        val containerId = if (helper isDotClass n) n.id
        else graph.container(nid).get
        containerId + ":" + n.id
      }
    }

    def subGraphArc(nid: NodeId, pos:String) = {
      val n = graph.getNode(nid)
      if (helper isDotSubgraph n) pos + "=cluster" + n.id + ", "
      else ""
    }

    val label = slabel match {
      case None => typeRelationShipLabel(edge)
      case Some(lbl) if typeRelationShipLabel(edge).isEmpty =>
        if(printConcreteUsesPerVirtualEdges)
          s"""label = "$lbl", """
        else ""
      case _ => sys.error("Label conflict")
    }

    if(!redOnly || status.color == redColor )
      arcs += (dotId(source) + " -> " + dotId(target) + "[ " +
        label +
        subGraphArc(source, "ltail") +
        subGraphArc(target, "lhead") +
        "style = " + style.line + ", arrowhead = " + style.arrowHead +
        ", color = " + status.color + ", penwidth = " + status.thickness+ "];");()


  }

  def violationStyle
  ( isViolation : Boolean ) :  ColorThickness = {
      if(isViolation) ColorThickness.violation
      else ColorThickness.regular
  }


  val typeRelationShipLabel : EdgeP => String =
    selectedUse match {
      case None => _ => ""
      case Some(selected) =>
        val labelMap : Map[EdgeP, String]= graph.kindType(selected.target) match {
          case TypeDecl =>
            val init = Map(DGEdge.toPair(selected) -> "TDecl")
            graph.typeMemberUsesOf(selected).foldLeft(init){
              (map, tm) => map. + (DGEdge.toPair(tm) -> "TMember")
            }
          case TypeMember
               | TypeConstructor =>
            val init = Map(DGEdge.toPair(selected) -> "TMember")
            graph.typeUsesOf(selected).foldLeft(init){
              (map, tm) => map. + (DGEdge.toPair(tm) -> "TDecl")
            }
          case TypeDeclAndTypeMember =>
            sys.error("selection kind unhandle [TODO] - DotPrinter.typeRelationShipLabel")
          case _ =>
            sys.error("this uses target kind should not happen")

        }
        e => labelMap get e match {
          case Some(l) => s"""label = "$l", """
          case None => ""
        }
    }



  def name( n : DGNode) : String = n match {
    case cn : ConcreteNode => html_safe(cn.name)
    case vn : VirtualNode => html_safe(vn.name(g))
  }

  def decorate_name(n : DGNode):String = {
    val sCter = graph.container(n.id)

    if (sCter.isDefined && concreteViolations.contains(Contains(sCter.get, n.id)))
      "<FONT COLOR=\"" + ColorThickness.violation.color + "\"><U>" + helper.namePrefix(n) + name(n) + idString(n.id) + "</U></FONT>"
    else helper.namePrefix(n) + name(n) + idString(n.id)
  }
  def printOrphanNode(nid : NodeId): Unit = {
    val n = graph.getNode(nid)
    val s = n.mapConcrete(cn => signatureString(cn.styp), "")

    writeln(s"""${n.id} [ label = "${n.kind} ${name(n)} ${idString(n.id)} $s" shape="rectangle" ]""")

  }

  def printNode(nid : NodeId): Unit =
    if(visibility.isVisible(nid)) {
      val n = graph.getNode(nid)
      if (helper isDotSubgraph n) printSubGraph(n)
      else if (helper isDotClass n) printClass(n.id)
      else printOrphanNode(nid)
    }



  def nodeSubGraphId(id : String) : String =
    "nodeCluster" + id
  def invisibleNode(id : String) : String =
    id + """ [ shape=none, label="" ];"""


  def printSubGraph(n : DGNode): Unit = {

    val label =
      s"""label=<<TABLE BORDER="0"><TR><TD BORDER="0" HREF="${n.id}" > ${decorate_name(n)}
         |</TD></TR></TABLE>>;""".stripMargin

      writeln(s"subgraph cluster${n.id} { $label color=black;")
      writeln(invisibleNode( nodeSubGraphId(n.id.toString)) )
      graph.content(n.id).foreach(printNode)
      writeln("}")

  }

  def printClass(nid: NodeId): Unit = {
    val n = graph.getNode(nid)
    def writeTableLine(nid: NodeId): Unit = {
      val n = graph.getNode(nid)
      val sig = n mapConcrete (cn => signatureString(cn.styp), "")


      writeln(s"""<TR><TD PORT="${n.id}" HREF="${n.id}" ALIGN="LEFT" BORDER="0">"""+
      //writeln(s"""<TR><TD PORT="${n.id}" ID="${n.id}" ALIGN="LEFT" BORDER="0">"""+
        decorate_name(n) + sig + "</TD></TR>")
    }

    val (fields, ctrs, mts, innerClasses) = helper splitDotClassContent (graph, n.id, visibility)

    writeln(s""" ${n.id} [ label = <<TABLE BGCOLOR="${helper.fillColor(n)}"> <TR> <TD PORT="${n.id}" HREF="${n.id}" BORDER="0"> <B>""" +
    //writeln(s""" ${n.id} [ label = <<TABLE BGCOLOR="${helper.fillColor(n)}"> <TR> <TD PORT="${n.id}" BORDER="0"> <B>""" +
      decorate_name(n) +" </B></TD></TR>")

    if(fields.nonEmpty || ctrs.nonEmpty || mts.nonEmpty) writeln("<HR/>")
    fields foreach writeTableLine
    if(fields.nonEmpty && ctrs.nonEmpty && mts.nonEmpty) writeln("<HR/>")
    ctrs foreach writeTableLine
    mts foreach writeTableLine

    writeln("</TABLE>>, shape = \"none\" ];")

    innerClasses foreach printClass

    /*graph.content(n.id).foreach { nc =>
      graph.users(nc).foreach(printUse(_, nc))
    }
    graph.users(n.id).foreach(printUse(_, n.id))

    graph.directSuperTypes(n.id).foreach(printArc(isaStyle, n.id, _, ColorThickness.regular))*/
  }

  def firstVisibleParent(nid : NodeId) : Option[NodeId] = 
    if(visibility.isVisible(nid)) Some(nid)
    else graph.container(nid) flatMap firstVisibleParent


  def recusivePackage(s : NodeId, t: NodeId) : Boolean =
    s == t && (helper isDotSubgraph graph.getNode(s))


  type ContainsViolationMap = Set[EdgeP]
  type IsViolation = Boolean
  def filterEdgeBasedOnVisibleNodes
  ( edges : Seq[EdgeP],
    edgeKind : DGEdge.EKind,
    virtInit : Map[EdgeP, Int] = Map(),
    virtViolationInit : ContainsViolationMap = Set()
    ) : (Seq[(EdgeP, IsViolation)], Map[EdgeP, Int], ContainsViolationMap) = {
    val (reg, virt, virtualViolations) =
      edges.foldLeft((Seq[(EdgeP, IsViolation)](), virtInit, virtViolationInit)) {
      case ((regulars, virtuals, m), (source, target)) =>
        (firstVisibleParent(source), firstVisibleParent(target)) match {
          case (Some(s), Some(t))
            if source == s && target == t =>
            val isViolation = concreteViolations.contains(edgeKind(source, target))
            (regulars :+ (((s, t), isViolation)), virtuals, m)

          case (Some(s), Some(t)) if printVirtualEdges =>
            val virtEdge = edgeKind(s, t)

            val numConcreteUses = (virtuals getOrElse ((s, t), 0)) + 1

            if(concreteViolations.contains(edgeKind(source, target)))
                (regulars, virtuals + ((s, t) -> numConcreteUses), m + ((s, t)))
            else if(!recusivePackage(s,t))
                (regulars, virtuals + ((s, t) -> numConcreteUses), m)
            else
                (regulars, virtuals, m)

          case _ => (regulars, virtuals, m)
        }
    }

    def filterConflict(reg : Seq[(EdgeP, IsViolation)], virtualViolations : ContainsViolationMap) = {
      reg.foldLeft((Seq[(EdgeP, IsViolation)](), virtualViolations)){
        case ((regulars, m), (e,iv)) =>
            (virt.contains(e), iv) match {
              case (false,_) =>  ((e,iv) +: regulars, m)
              case (true, false) => (regulars,  m)
              case (true, true) => (regulars,  m + e)
            }
      }
    }
//    def filterConflict(reg : Seq[(EdgeP, IsViolation)], virtualViolations : ContainsViolationMap) = (reg, virtualViolations)

    //reg.fol
    val (reg1, virtualViolations1) = filterConflict(reg, virtualViolations)
    (reg1, virt, virtualViolations1)
  }

  def apply(): Unit = {
    writeln("digraph G{")
    writeln("rankdir=LR; ranksep=equally; compound=true")

    graph.content(graph.rootId).foreach(printNode)
    graph.nodesId.foreach{nid => if(graph.container(nid).isEmpty) printNode(nid)}


    val (regularsIsa, virt0, virtualViolations0) = filterEdgeBasedOnVisibleNodes(graph.isaList, IsaK)

    regularsIsa.foreach {
      case (e, v) => printArc(violationStyle(v),isaStyle)(e)
    }

    val(reg, virt, virtualViolations) = filterEdgeBasedOnVisibleNodes(graph.usesList, UsesK, virt0, virtualViolations0)

    reg.foreach{ case (e, v) =>  printArc(violationStyle(v),usesStyle)(e) }


    virt.foreach {
      case (e, numConcretes) =>
        printArc(violationStyle(virtualViolations contains e),virtualUse,
          Some(numConcretes.toString))(e)
    }

    arcs.foreach(writeln)

    writeln("}")

    writer.close()

  }
}
