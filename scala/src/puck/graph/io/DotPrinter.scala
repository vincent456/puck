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



  def print[K <: NodeKind[K]](writer: BufferedWriter, graph : AccessGraph[K],
                                                  helper : DotHelper[K],
                                                  printId : Boolean,
                                                  printSignatures : Boolean = false,
                                                  searchRoots : Boolean = false,
                                                  selectedUse : Option[AGEdge[K]] = None){

    type NodeType = AGNode[K]

    val idPrinter =
      if(printId) (id:Int) => " (" + id + ")"
      else (_:Int) => ""

    def writeln(str:String){
      writer write str
      writer newLine()
    }

    val violations = selectedUse match{
      case None => graph.violations
      case Some(_) => List()
    }
    /*
     * dot -Tpng give a wrong drawing of the graph when mixing nodes and arcs
     * in the dot file. We stock the arcs and print them separately at the end
     */
    val arcs = scala.collection.mutable.Buffer[String]()
    def printArc(style : Style, source : NodeType, target : NodeType,
                 status: ColorThickness){
      //val (lineStyle, headStyle) = style
      //val (color, thickness) = status
      //println("print arc "+ source.nameTypeString + " -> " + target.nameTypeString)
      def dotId(n: NodeType) : String =
        if(helper isDotSubgraph n.kind) n.id.toString
        else{
          val containerId = if(helper isDotClass n.kind) n.id
          else n.container.id
          containerId + ":" + n.id
        }


      def subGraphArc(n: NodeType, pos:String) =
        if(helper isDotSubgraph n.kind) pos + "=cluster" + n.id + ", "
        else ""

      /*writeln*/
      arcs += (dotId(source) + " -> " + dotId(target) + "[ " +
        subGraphArc(source, "ltail") +
        subGraphArc(target, "lhead") +
        "style = " + style.line + ", arrowhead = " + style.arrowHead +
        ", color = " + status.color + ", penwidth = " + status.thickness+ "];")

    }

    val printUsesViolations = (source : NodeType, target : NodeType) =>
      if(!source.isa(target)) //TODO remove test. quickfix to avoid dot crash
        printArc(usesStyle, source, target,
          if(violations.contains(AGEdge.uses(source, target)))
            ColorThickness.violation
          else ColorThickness.regular )

    val printUse = selectedUse match {
      case None => printUsesViolations
      case Some(selected) =>  (source: NodeType, target: NodeType) =>
        val printed = AGEdge.uses(source, target)
        val ct = if (printed == selected) ColorThickness.selected
        else if (selected.source.primaryUses.getOrEmpty(selected.target).exists(_ == printed))
          ColorThickness.dominant
        else if (selected.source.sideUses.getOrEmpty(selected.target).exists(_ == printed))
          ColorThickness.dominated
        else ColorThickness.regular


        printArc(usesStyle, source, target, ct)

    }

    def decorate_name(n : NodeType):String =
      if (violations.contains(AGEdge.contains(n.container, n)))
        "<FONT COLOR=\"" + ColorThickness.violation.color + "\"><U>" + helper.namePrefix(n.kind)+ n.name + idPrinter(n.id) +"</U></FONT>"
      else helper.namePrefix(n.kind)+ n.name + idPrinter(n.id)

    def printNode(n : NodeType){
      if(helper isDotSubgraph n.kind) printSubGraph(n)
      else if(helper isDotClass n.kind) printClass(n)
    }

    def printSubGraph(n : NodeType){
      List("subgraph cluster" + n.id + " {",
        "label=\"" + decorate_name(n) +"\";",
        "color=black;") foreach writeln

      if(n.content.isEmpty) writeln(n.id + "[label=\"\" shape=none ]")
      else
        n.content.foreach(printNode)

      writeln("}")

      n.users.foreach(printUse(_, n))
    }

    def printClass(n:NodeType){

      def writeTableLine(n:NodeType){
        val sig = if (printSignatures) n.kind match {
         case k : HasType[_,_] => " : " + k.`type`.toString.replaceAllLiterally(">", "&gt;") + " "
         case _ => ""
        }
        else ""

        writeln("<TR><TD PORT=\"" +n.id + "\" ALIGN=\"LEFT\" BORDER=\"0\">"+
          decorate_name(n) + sig + "</TD></TR>")
      }

      val (fields, ctrs, mts, innerClasses) = helper splitDotClassContent n

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
        nc.users.foreach(printUse(_, nc))
      }
      n.users.foreach(printUse(_, n))
      n.superTypes.foreach(printArc(isaStyle, n, _, ColorThickness.regular))
    }



    writeln("digraph G{")
    writeln("rankdir=LR; ranksep=equally; compound=true")

    if(!searchRoots)
      graph.root.content.foreach(printNode)
    else
      graph.nodes.foreach{n => if(n.container == null) printNode(n)}

    arcs.foreach(writeln)

    writeln("}")

    writer.close()

  }
}
