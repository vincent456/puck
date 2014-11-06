package puck.graph.io

import java.io.BufferedWriter

import puck.graph._

/**
 * Created by lorilan on 13/08/14.
 */
object DotPrinter {

  class Style(val line: String, val arrowHead : String)
  val isaStyle = new Style("dashed", "empty")
  val containsStyle = new Style("dashed", "open")
  val usesStyle = new Style("bold", "normal")

  class ColorThickness(val color : String, val thickness : Int)

  object ColorThickness {
    val regular = new ColorThickness("black", 1)
    val violation = new ColorThickness("red", 5)

    val dominant = new ColorThickness("blue", 2)
    val dominated = new ColorThickness("green", 2)
    val selected = new ColorThickness("black", 5)
  }



  def print(writer: BufferedWriter,
                              graph : AccessGraph,
                              helper : DotHelper,
                              printId : Boolean,
                              printSignatures : Boolean = false,
                              searchRoots : Boolean = false,
                              selectedUse : Option[AGEdge] = None){

    type NIdT = NodeId

    val idPrinter =
      if(printId) (id:Int) => " (" + id + ")"
      else (_:Int) => ""

    def writeln(str:String){
      writer write str
      writer newLine()
    }

    val violations = selectedUse match{
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
          else graph.container(nid)
          containerId + ":" + n.id
        }
      }

      def subGraphArc(nid: NIdT, pos:String) = {
        val n = graph.getNode(nid)
        if (helper isDotSubgraph n.kind) pos + "=cluster" + n.id + ", "
        else ""
      }

      /*writeln*/
      arcs += (dotId(source) + " -> " + dotId(target) + "[ " +
        subGraphArc(source, "ltail") +
        subGraphArc(target, "lhead") +
        "style = " + style.line + ", arrowhead = " + style.arrowHead +
        ", color = " + status.color + ", penwidth = " + status.thickness+ "];")

    }

    val printUsesViolations = (source : NIdT, target : NIdT) =>
      if(! graph.isa(source, target)) //TODO remove test. quickfix to avoid dot crash
        printArc(usesStyle, source, target,
          if(violations.contains(AGEdge.uses(source, target)))
            ColorThickness.violation
          else ColorThickness.regular )

    val printUse = selectedUse match {
      case None => printUsesViolations
      case Some(selected) =>  (source: NIdT, target: NIdT) =>
        val printed = AGEdge.uses(source, target)
        val ct = if (printed == selected) ColorThickness.selected
        else if (graph.dominates(printed, selected))
          ColorThickness.dominant
        else if (graph.dominates(selected, printed))
          ColorThickness.dominated
        else ColorThickness.regular


        printArc(usesStyle, source, target, ct)

    }

    def decorate_name(n : AGNode):String =
      if (violations.contains(AGEdge.contains(n.container, n.id)))
        "<FONT COLOR=\"" + ColorThickness.violation.color + "\"><U>" + helper.namePrefix(n.kind) + n.name + idPrinter(n.id) + "</U></FONT>"
      else helper.namePrefix(n.kind) + n.name + idPrinter(n.id)


    def printNode(nid : NodeId){
      val n = graph.getNode(nid)
      if(helper isDotSubgraph n.kind) printSubGraph(n)
      else if(helper isDotClass n.kind) printClass(n.id)
    }

    def printSubGraph(n : AGNode){
      List("subgraph cluster" + n.id + " {",
        "label=\"" + decorate_name(n) +"\";",
        "color=black;") foreach writeln

      if(n.content.isEmpty) writeln(n.id + "[label=\"\" shape=none ]")
      else
        n.content.foreach(printNode)

      writeln("}")

      n.users.foreach(printUse(_, n.id))
    }

    def printClass(nid: NodeId){
      val n = graph.getNode(nid)
      def writeTableLine(nid: NodeId){
        val n = graph.getNode(nid)
        val sig = if (printSignatures)
          n.styp.mkString(graph).replaceAllLiterally(">", "&gt;") + " "
        else ""

        writeln("<TR><TD PORT=\"" +n.id + "\" ALIGN=\"LEFT\" BORDER=\"0\">"+
          decorate_name(n) + sig + "</TD></TR>")
      }

      val (fields, ctrs, mts, innerClasses) = helper splitDotClassContent (graph, n.id)

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

      n.content.foreach { nc =>
        graph.getNode(nc).users.foreach(printUse(_, nc))
      }
      n.users.foreach(printUse(_, n.id))
      n.directSuperTypes.foreach(printArc(isaStyle, n.id, _, ColorThickness.regular))
    }



    writeln("digraph G{")
    writeln("rankdir=LR; ranksep=equally; compound=true")

    if(!searchRoots)
      graph.content(graph.rootId).foreach(printNode)
    else
      graph.nodes.foreach{n => if(n.isRoot) printNode(n.id)}

    arcs.foreach(writeln)

    writeln("}")

    writer.close()

  }
}
