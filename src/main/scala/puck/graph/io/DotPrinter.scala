package puck.graph
package io


import java.io.BufferedWriter

import scala.collection.mutable

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
    val regular = new ColorThickness("black", 2)
    val violation = new ColorThickness("red", 5)

    val dominant = new ColorThickness("blue", 2)
    val dominated = new ColorThickness("green", 2)
    val selected = new ColorThickness("black", 5)
  }

}

case class PrintingOptions(visibility : VisibilitySet,
                           printId : Boolean = false,
                           printSignatures : Boolean = false,
                           selectedUse : Option[DGEdge] = None)

import DotPrinter._
class DotPrinter
( writer: BufferedWriter,
  graph : DependencyGraph,
  helper : DotHelper,
  printingOptions: PrintingOptions){

  import printingOptions._

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

  private val violations = selectedUse match{
    case None => graph.violations()
    case Some(_) => Seq()
  }


  /*
 * dot -Tpng give a wrong drawing of the graph when mixing nodes and arcs
 * in the dot file. We stock the arcs and print them separately at the end
 */
  val arcs = scala.collection.mutable.Buffer[String]()

  def printArc(style : Style, source : NodeId, target : NodeId,
               status: ColorThickness): Unit = {
    //val (lineStyle, headStyle) = style
    //val (color, thickness) = status
    //println("print arc "+ source.nameTypeString + " -> " + target.nameTypeString)
    def dotId(nid: NodeId) : String = {
      val n = graph.getNode(nid)
      if (helper isDotSubgraph n) n.id.toString
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

    if(visibility.isVisible(source) && visibility.isVisible(target))
    arcs += (dotId(source) + " -> " + dotId(target) + "[ " +
      subGraphArc(source, "ltail") +
      subGraphArc(target, "lhead") +
      "style = " + style.line + ", arrowhead = " + style.arrowHead +
      ", color = " + status.color + ", penwidth = " + status.thickness+ "];")

    ()
  }



  val printUsesViolations = (source : NodeId, target : NodeId) =>
    if(!graph.isa(source, target)) //TODO remove test. quickfix to avoid dot crash
      printArc(usesStyle, source, target,
        if(violations.contains(DGEdge.uses(source, target)))
          ColorThickness.violation
        else ColorThickness.regular )

  val printUse = selectedUse match {
    case None => printUsesViolations
    case Some(selected) =>  (source: NodeId, target: NodeId) =>
      val printed = DGEdge.uses(source, target)
      val ct = if (printed == selected) ColorThickness.selected
      else if (graph.dominates(printed, selected))
        ColorThickness.dominant
      else if (graph.dominates(selected, printed))
        ColorThickness.dominated
      else ColorThickness.regular


      printArc(usesStyle, source, target, ct)

  }

  def name( n : DGNode) : String = n match {
    case cn : ConcreteNode => html_safe(cn.name)
    case vn : VirtualNode => html_safe(vn.name(g))
  }

  def decorate_name(n : DGNode):String = {
    val sCter = graph.container(n.id)

    if (sCter.isDefined && violations.contains(DGEdge.contains(sCter.get, n.id)))
      "<FONT COLOR=\"" + ColorThickness.violation.color + "\"><U>" + helper.namePrefix(n) + name(n) + idString(n.id) + "</U></FONT>"
    else helper.namePrefix(n) + name(n) + idString(n.id)
  }
  def printOrphanNode(nid : NodeId): Unit = {
    val n = graph.getNode(nid)
    val s = n.mapConcrete(cn => signatureString(cn.styp), "")

    writeln(s"""${n.id} [ label = "${n.kind} ${name(n)} ${idString(n.id)} $s" ]""")

  }

  def printNode(nid : NodeId): Unit = {
    if(visibility.isVisible(nid)) {
      val n = graph.getNode(nid)
      if (helper isDotSubgraph n) printSubGraph(n)
      else if (helper isDotClass n) printClass(n.id)
      else printOrphanNode(nid)
    }
  }



  def printSubGraph(n : DGNode): Unit = {

    val label =
      s"""label=<<TABLE BORDER="0"><TR><TD BORDER="0" HREF="${n.id}" > ${decorate_name(n)}
         |</TD></TR></TABLE>>;""".stripMargin
    //s"""label=<<TABLE BORDER="0"><TR><TD BORDER="0" ID="${n.id}" > ${decorate_name(n)}
    // </TD></TR></TABLE>>;""",

    if(isEmpty(n.id)) writeln(s"${n.id} [ $label shape=rectangle ]")
    else{
      writeln(s"subgraph cluster${n.id} { $label color=black;")
      graph.content(n.id).foreach(printNode)
      writeln("}")
    }



    graph.users(n.id).foreach(printUse(_, n.id))
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

    graph.content(n.id).foreach { nc =>
      graph.users(nc).foreach(printUse(_, nc))
    }
    graph.users(n.id).foreach(printUse(_, n.id))
    graph.directSuperTypes(n.id).foreach(printArc(isaStyle, n.id, _, ColorThickness.regular))
  }

  def apply(): Unit = {
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
