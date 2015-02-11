package puck.graph
package io

import scalaz._
import Scalaz._


import java.io.BufferedWriter

/**
 * Created by lorilan on 13/08/14.
 */
object DotPrinter {

  class Style(val line: String, val arrowHead: String)

  val isaStyle = new Style("dashed", "empty")
  val containsStyle = new Style("dashed", "open")
  val usesStyle = new Style("bold", "normal")

  class ColorThickness(val color: String, val thickness: Int)

  object ColorThickness {
    val regular = new ColorThickness("black", 1)
    val violation = new ColorThickness("red", 5)

    val dominant = new ColorThickness("blue", 2)
    val dominated = new ColorThickness("green", 2)
    val selected = new ColorThickness("black", 5)
  }

}

import DotPrinter._
class DotPrinter
( writer: BufferedWriter,
  graph : DependencyGraph,
  visibility : VisibilitySet,
  helper : DotHelper,
  printId : Boolean,
  printSignatures : Boolean = false,
  searchRoots : Boolean = false,
  selectedUse : Option[DGEdge] = None){

  implicit val g = graph
  type NIdT = NodeId

  val idString : Int => String =
    if(printId) id => " (" + id + ")"
    else _ => ""

  def html_safe(str : String) : String = str.replaceAllLiterally(">", "&gt;").replaceAllLiterally("<", "&lt;")

  val signatureString : TypeHolder => String =
    if (printSignatures)
      styp => {
        import ShowDG._
        showDG[TypeHolder](graph).shows(styp).replaceAllLiterally(">", "&gt;") + " "
      }
    else _ => ""


  def writeln(str:String){
    writer write str
    writer newLine()
  }

  private val violations = selectedUse match{
    case None => graph.violations()
    case Some(_) => Seq()
  }


  /*
 * dot -Tpng give a wrong drawing of the graph when mixing nodes and arcs
 * in the dot file. We stock the arcs and print them separately at the end
 */
  val arcs = scala.collection.mutable.Buffer[String]()

  def printArc(style : Style, source : NIdT, target : NIdT,
               status: ColorThickness){
    //val (lineStyle, headStyle) = style
    //val (color, thickness) = status
    //println("print arc "+ source.nameTypeString + " -> " + target.nameTypeString)
    def dotId(nid: NIdT) : String = {
      val n = graph.getNode(nid)
      if (helper isDotSubgraph n.kind) n.id.toString
      else {
        val containerId = if (helper isDotClass n.kind) n.id
        else graph.container(nid).get
        containerId + ":" + n.id
      }
    }

    def subGraphArc(nid: NIdT, pos:String) = {
      val n = graph.getNode(nid)
      if (helper isDotSubgraph n.kind) pos + "=cluster" + n.id + ", "
      else ""
    }

    if(visibility.isVisible(source) && visibility.isVisible(target))
    arcs += (dotId(source) + " -> " + dotId(target) + "[ " +
      subGraphArc(source, "ltail") +
      subGraphArc(target, "lhead") +
      "style = " + style.line + ", arrowhead = " + style.arrowHead +
      ", color = " + status.color + ", penwidth = " + status.thickness+ "];")

  }



  val printUsesViolations = (source : NIdT, target : NIdT) =>
    if(!graph.isa(source, target)) //TODO remove test. quickfix to avoid dot crash
      printArc(usesStyle, source, target,
        if(violations.contains(DGEdge.uses(source, target)))
          ColorThickness.violation
        else ColorThickness.regular )

  val printUse = selectedUse match {
    case None => printUsesViolations
    case Some(selected) =>  (source: NIdT, target: NIdT) =>
      val printed = DGEdge.uses(source, target)
      val ct = if (printed == selected) ColorThickness.selected
      else if (graph.dominates(printed, selected))
        ColorThickness.dominant
      else if (graph.dominates(selected, printed))
        ColorThickness.dominated
      else ColorThickness.regular


      printArc(usesStyle, source, target, ct)

  }

  def decorate_name(n : DGNode):String = {
    val sCter = graph.container(n.id)

    val name = html_safe(n.name)

    if (sCter.isDefined && violations.contains(DGEdge.contains(sCter.get, n.id)))
      "<FONT COLOR=\"" + ColorThickness.violation.color + "\"><U>" + helper.namePrefix(n.kind) + name + idString(n.id) + "</U></FONT>"
    else helper.namePrefix(n.kind) + name + idString(n.id)
  }
  def printOrphanNode(nid : NodeId): Unit = {
    val n = graph.getNode(nid)
    writeln(n.id + " [ label = \"" + n.kind + "  " + html_safe(n.name) + idString(n.id) + signatureString(n.styp)+"\" ]")
  }

  def printNode(nid : NodeId){
    if(visibility.isVisible(nid)) {
      val n = graph.getNode(nid)
      if (helper isDotSubgraph n.kind) printSubGraph(n)
      else if (helper isDotClass n.kind) printClass(n.id)
      else printOrphanNode(nid)
    }
  }



  def printSubGraph(n : DGNode){
    List("subgraph cluster" + n.id + " {",
      "label=\"" + decorate_name(n) +"\";",
      "color=black;") foreach writeln

    if(graph.content(n.id).isEmpty) writeln(n.id + "[label=\"\" shape=none ]")
    else
      graph.content(n.id).foreach(printNode)

    writeln("}")

    graph.users(n.id).foreach(printUse(_, n.id))
  }

  def printClass(nid: NodeId){
    val n = graph.getNode(nid)
    def writeTableLine(nid: NodeId){
      val n = graph.getNode(nid)
      val sig = signatureString(n.styp)

      writeln("<TR><TD PORT=\"" +n.id + "\" ALIGN=\"LEFT\" BORDER=\"0\">"+
        decorate_name(n) + sig + "</TD></TR>")
    }

    val (fields, ctrs, mts, innerClasses) = helper splitDotClassContent (graph, n.id, visibility)

    writeln(n.id + " [ label = <<TABLE BGCOLOR=\"" + helper.fillColor(n.kind)+
      "\"> <TR> <TD PORT=\""+ n.id+"\" BORDER=\"0\"> <B>" +
      decorate_name(n) +" </B></TD></TR>")

    if(fields.nonEmpty || ctrs.nonEmpty || mts.nonEmpty) writeln("<HR/>")
    fields foreach writeTableLine
    if(fields.nonEmpty && ctrs.nonEmpty && mts.nonEmpty) writeln("<HR/>")
    ctrs foreach writeTableLine
    mts foreach writeTableLine

    writeln("</TABLE>>, shape = \"none\" ];")

    innerClasses foreach printClass

    graph.content(n.id).foreach { nc =>
      graph.users(nc).foreach(printUse(_, nc))
    }
    graph.users(n.id).foreach(printUse(_, n.id))
    graph.directSuperTypes(n.id).foreach(printArc(isaStyle, n.id, _, ColorThickness.regular))
  }

  def apply(){
    writeln("digraph G{")
    writeln("rankdir=LR; ranksep=equally; compound=true")

    graph.content(graph.rootId).foreach(printNode)
    visibility.setVisibility(DependencyGraph.rootId, Hidden)
    graph.nodesId.foreach{nid => if(graph.container(nid).isEmpty) printNode(nid)}

    arcs.foreach(writeln)

    writeln("}")

    writer.close()

  }
}
