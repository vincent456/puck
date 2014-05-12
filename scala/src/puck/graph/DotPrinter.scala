package puck.graph

import _root_.java.io.BufferedWriter
import scala.None

/**
 * Created by lorilan on 07/05/14.
 */

trait DotHelper{
  def isDotSubgraph(k:NodeKind) : Boolean
  def isDotClass(k:NodeKind) : Boolean
  def fillColor(k:NodeKind) : String
  def namePrefix(k:NodeKind) : String
  def splitDotClassContent(n: AGNode) : (Iterable[AGNode], Iterable[AGNode], Iterable[AGNode] , Iterable[AGNode])
  //with java ((fields, Constructors, Methods), inner classes)
}

object DotPrinter {

  //arrow styles (line, head)
  val isaStyle = ("dashed", "empty")
  val containsStyle = ("dashed", "open")
  val usesStyle = ("bold", "normal")

  //(color, thickness)
  val correctStatus = ("black", "1")
  val violationStatus = ("red", "5")

  def print(writer: BufferedWriter, graph : AccessGraph,
            helper : DotHelper, printId : Boolean){

     val idPrinter =
      if(printId) (id:Int) => " (" + id + ")"
      else (_:Int) => ""

    def writeln(str:String){
      writer write str
      writer newLine()
    }

    def printNode(n:AGNode){
      //println("print node "+ n.nameTypeString)
      if(helper isDotSubgraph n.kind) printSubGraph(n)
      else if(helper isDotClass n.kind) printClass(n)
    }

    def printSubGraph(n:AGNode){
      List("subgraph cluster" + n.id + " {",
        "label=\"" +helper.namePrefix(n.kind)+ n.name + idPrinter(n.id) +"\";",
        "color=black;") foreach writeln

      if(n.isContentEmpty) writeln(n.id + "[label=\"\" shape=none ]")
      else for(n <- n.content) printNode(n)

      writeln("}")

      n.users.foreach(printArc(usesStyle, _, n, correctStatus))
    }

    def printClass(n:AGNode){

      def writeTableLine(n:AGNode){
        writeln("<TR><TD PORT=\"" +n.id + "\" ALIGN=\"LEFT\" BORDER=\"0\">"+
          n.name + idPrinter(n.id) +"</TD></TR>")
      }

      val (fields, ctrs, mts, innerClasses) = helper splitDotClassContent n

      writeln(n.id + " [ label = <<TABLE BGCOLOR=\"" + helper.fillColor(n.kind)+
        "\"> <TR> <TD PORT=\""+ n.id+"\" BORDER=\"0\"> <B>" +
        helper.namePrefix(n.kind)+ n.name + idPrinter(n.id) +" </B></TD></TR>")

      if(!fields.isEmpty || !ctrs.isEmpty || ! mts.isEmpty) writeln("<HR/>")
      fields foreach writeTableLine
      if(!fields.isEmpty && !ctrs.isEmpty && !mts.isEmpty) writeln("<HR/>")
      ctrs foreach writeTableLine
      mts foreach writeTableLine

      writeln("</TABLE>>, shape = \"none\" ];")

      innerClasses foreach printClass

      n.content foreach{
        (n:AGNode) =>
          n.users.foreach(printArc(usesStyle, _, n, correctStatus))
      }
      n.users.foreach(printArc(usesStyle, _, n, correctStatus))
      n.superTypes.foreach(printArc(isaStyle, _, n, correctStatus))
    }


    def printArc(style :(String, String), source:AGNode, target:AGNode,
                 status:(String, String)){
      //val (lineStyle, headStyle) = style
      //val (color, thickness) = status
      //println("print arc "+ source.nameTypeString + " -> " + target.nameTypeString)
      def dotId(n: AGNode) : String =
        if(helper isDotSubgraph n.kind) n.id.toString
        else{
          val containerId = if(helper isDotClass n.kind) n.id
          else n.container match {
            case None => throw new Error("node " + n.nameTypeString + " should have a container")
            case Some(ctr) => ctr.id
          }
          containerId + ":" + n.id
        }


      def subGraphArc(n: AGNode, pos:String) =
        if(helper isDotSubgraph n.kind) pos+"=cluster"+n.id+", "
        else ""

      writeln(dotId(source) + " -> " + dotId(target) + "[ " +
        subGraphArc(source, "ltail") +
        subGraphArc(target, "lhead") +
        "style=" + style._1 + ", arrowhead=" + style._2 +
        ", color =" + status._1 + ", penwidth=" + status._2+ "];")

    }

    writeln("digraph G{")
    writeln("rankdir=LR; ranksep=2; compound=true")

    graph.root.content.foreach(printNode)

    writeln("}")
    writer.close()

  }
}
