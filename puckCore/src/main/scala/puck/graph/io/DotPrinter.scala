package puck.graph
package io


import java.io._

import scala.collection.mutable
import scala.concurrent.Future
import scala.sys.process.Process
import scala.util.Try

import scala.concurrent.ExecutionContext.Implicits.global


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



//  def makeImage(graph : DependencyGraph,
//                graphUtils: GraphUtils,
//                printingOptions: PrintingOptions,
//                sOutput : Option[OutputStream] = None,
//                outputFormat : DotOutputFormat = Png)
//               (finish : Try[Int] => Unit = {case _ => ()}) : Unit = {
//    //TODO fix bug when chaining the two function with a pipe
////    makeDot(graph, graphUtils.dotHelper, printingOptions, new FileWriter(pathWithoutSuffix + ".dot"))
////
////    finish(Success(convertDot(graphvizDot, pathWithoutSuffix, sInput = None, sOutput, outputFormat)))
//
//    val pipedOutput = new PipedOutputStream()
//    val pipedInput = new PipedInputStream(pipedOutput)
//
//    Future {
//      convertDot(graphvizDot, pathWithoutSuffix, sInput = Some(pipedInput), sOutput, outputFormat)
//    } onComplete finish
//
//    genDot(graph, graphUtils.dotHelper, printingOptions, writer = new OutputStreamWriter(pipedOutput))
//  }

  type DotProcessBuilder = scala.sys.process.ProcessBuilder
  // relies on dot directory being in the PATH variable
  def dotProcessBuilderFromFile(pathWithoutSuffix : String, outputFormat: DotOutputFormat) : DotProcessBuilder =
    Process(List("dot", "-T" + outputFormat, pathWithoutSuffix + ".dot"))

  def dotProcessBuilderFromFileToFile(pathWithoutSuffix : String, outputFormat: DotOutputFormat) : DotProcessBuilder =
    Process(List("dot", "-O", "-T" + outputFormat, pathWithoutSuffix + ".dot"))
  // processBuilder #> new File( pathWithoutSuffix + "." + outputFormat)).!

  def dotProcessBuilderFromInputStream(input : InputStream, outputFormat: DotOutputFormat) : DotProcessBuilder=
    Process(List("dot", "-T" + outputFormat)) #< input


  def genDot
  ( graph : DependencyGraph,
    dotHelper: DotHelper,
    printingOptions: PrintingOptions,
    writer : OutputStreamWriter) : Unit = {
    val printer = new DotPrinter(new BufferedWriter(writer), graph, dotHelper, printingOptions)
    printer.print()
  }
  def genDotFile
  ( graph : DependencyGraph,
    dotHelper: DotHelper,
    printingOptions: PrintingOptions,
    pathWithoutSuffix : String) : Unit =
    genDot(graph, dotHelper, printingOptions, new FileWriter(pathWithoutSuffix + ".dot"))


  def genImage
  (graph : DependencyGraph,
   dotHelper: DotHelper,
   printingOptions: PrintingOptions,
   outputFormat: DotOutputFormat,
   output : OutputStream)
  (finish : Try[Int] => Unit = {case _ => ()}) = {

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    Future {
      (dotProcessBuilderFromInputStream(pipedInput, outputFormat) #> output).!
    } onComplete finish

    genDot(graph, dotHelper, printingOptions, writer = new OutputStreamWriter(pipedOutput))
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
        (graph, styp).shows.replaceAllLiterally(">", "&gt;") + " "
      }
    else _ => ""


  def writeln(str:String) : Unit = {
    writer write str
    writer newLine()
  }

  def transformParamOrDefToDecl(e : DGEdge) : DGEdge =
      graph.kindType(e.source) match {
          case Parameter | ValueDef => e.copy(source = graph.container_!(e.source))
          case _ => e
      }

  private val concreteViolations : Seq[DGEdge] = graph.violations map transformParamOrDefToDecl distinct

//  import scalaz.syntax.show._
//  import ShowDG._
//
//  println(concreteViolations map (e => (g,e).shows) mkString("[",",\n","]"))

/*
 * dot -Tpng give a wrong drawing of the graph when mixing nodes and arcs
 * in the dot file. We stock the arcs and print them separately at the end
 */
  val arcs = scala.collection.mutable.Buffer[String]()


  def printArc
  ( status : ColorThickness,
    style : Style,
    slabel : Option[String] = None): NodeIdP => Unit = {
    case edge @ (source, target) =>

    def dotId(nid: NodeId) : String = {
      val n = graph.getNode(nid)
      n.kind.kindType match {
        case NameSpace =>  nodeSubGraphId(n.id.toString)
        case ValueDef | Parameter => dotId(graph.container_!(nid))
        case TypeDecl => n.id + ":" + n.id
        case _ =>
          graph.container(nid) match {
            case Some(id) => id + ":" + n.id
            case None => n.id.toString
              //throw new PuckError(showDG[NodeId](g).show(nid) + " has no container !")
          }
      }
    }

    def subGraphArc(nid: NodeId, pos:String) = {
      val n = graph.getNode(nid)
      if (n.kind.kindType == NameSpace) pos + "=cluster" + n.id + ", "
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


  val typeRelationShipLabel : NodeIdP => String =
    selectedUse match {
      case None => _ => ""
      case Some(selected) =>
        val labelMap : Map[NodeIdP, String]= graph.kindType(selected.target) match {
          case TypeDecl =>
            val init = Map(DGEdge.toPair(selected) -> "TDecl")
            graph.typeMemberUsesOf(selected).foldLeft(init){
              (map, tm) => map. + (DGEdge.toPair(tm) -> "TMember")
            }
          case InstanceValueDecl
               | TypeConstructor =>
            val init = Map(DGEdge.toPair(selected) -> "TMember")
            graph.typeUsesOf(selected).foldLeft(init){
              (map, tm) => map. + (DGEdge.toPair(tm) -> "TDecl")
            }
          case InstanceTypeDecl =>
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
    val s = n.mapConcrete(cn => signatureString(graph.structuredType(cn.id)), "")

    writeln(s"""${n.id} [ label = "${n.kind} ${name(n)} ${idString(n.id)} $s" shape="rectangle" ]""")

  }

  def printNode(nid : NodeId): Unit =
    if(visibility.isVisible(nid)) {
      val n = graph.getNode(nid)
      n.kind.kindType match {
        case NameSpace => printSubGraph(n)
        case TypeDecl =>  printClass(n.id)
        case ValueDef => ()
        case _ => printOrphanNode(nid)
      }
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
      val sig = n mapConcrete (cn => signatureString(graph.structuredType(cn.id)), "")

      val (itb, ite) = n.kind.kindType match {
        case InstanceValueDecl if n.definition(g).isEmpty =>
          ("<I>", "</I>")
        case StaticValueDecl  =>
          ("<I>&lt;&lt;static&gt;&gt;</I>", "")
        case _ => ("","")
      }

      writeln(s"""<TR><TD PORT="${n.id}" HREF="${n.id}" ALIGN="LEFT" BORDER="0">"""+ itb +
      //writeln(s"""<TR><TD PORT="${n.id}" ID="${n.id}" ALIGN="LEFT" BORDER="0">"""+ itb +
        decorate_name(n) + sig + ite + "</TD></TR>")
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

  }

  def firstVisibleParent(nid : NodeId) : Option[NodeId] = 
    if(visibility.isVisible(nid)) Some(nid)
    else graph.container(nid) flatMap firstVisibleParent


  def recusivePackage(s : NodeId, t: NodeId) : Boolean =
    s == t && (graph.getNode(s).kind.kindType == NameSpace)


  type ContainsViolationMap = Set[NodeIdP]
  type IsViolation = Boolean
  def filterAllEdgeBasedOnVisibleNodesAndFlagViolations
  ( edges : Seq[DGEdge],
    virtInit : Map[DGEdge, Int] = Map(),
    virtViolationInit : ContainsViolationMap = Set()
    ) : (Seq[(DGEdge, IsViolation)], Map[DGEdge, Int], ContainsViolationMap) = {

    val (reg, virt, virtualViolations) =
      (edges map transformParamOrDefToDecl).foldLeft((Seq[(DGEdge, IsViolation)](), virtInit, virtViolationInit)) {
      case ((regulars, virtuals, m), e @ DGEdge(k, source, target)) =>
        (firstVisibleParent(source), firstVisibleParent(target)) match {
          case (Some(s), Some(t))
            if source == s && target == t =>
            val isViolation = concreteViolations.contains(e)
            (regulars :+ ((k(s, t), isViolation)), virtuals, m)

          case (Some(s), Some(t)) if printVirtualEdges =>
            val virtEdge = k(s, t)

            val numConcreteUses = (virtuals getOrElse (k(s, t), 0)) + 1

            if(concreteViolations.contains(e))
                (regulars, virtuals + (k(s, t) -> numConcreteUses), m + k(s, t))
            else if(!recusivePackage(s,t))
                (regulars, virtuals + (k(s, t) -> numConcreteUses), m)
            else
                (regulars, virtuals, m)

          case _ => (regulars, virtuals, m)
        }
    }

    def filterDuplication(reg : Seq[(DGEdge, IsViolation)], virtualViolations : ContainsViolationMap) = {
      reg.foldLeft((Seq[(DGEdge, IsViolation)](), virtualViolations)){
        case ((regulars, m), (e,iv)) =>
            (virt.contains(e), iv) match {
              case (false,_) =>  ((e,iv) +: regulars, m)
              case (true, false) => (regulars,  m)
              case (true, true) => (regulars,  m + e)
            }
      }
    }
//    def filterDuplication(reg : Seq[(EdgeP, IsViolation)], virtualViolations : ContainsViolationMap) = (reg, virtualViolations)

    //reg.fol
    val (reg1, virtualViolations1) = filterDuplication(reg, virtualViolations)
    (reg1, virt, virtualViolations1)
  }

  def filterEdgeBasedOnVisibleNodes
  ( edges : Seq[DGEdge],
    virtInit : Map[NodeIdP, Int] = Map()
    ) : (Seq[DGEdge], Map[NodeIdP, Int]) = {

    //val (reg, virt) =
      (edges map transformParamOrDefToDecl).foldLeft((Seq[DGEdge](), virtInit)) {
        case ((regulars, virtuals), DGEdge(k, source, target)) =>
          (firstVisibleParent(source), firstVisibleParent(target)) match {
            case (Some(s), Some(t))
              if source == s && target == t =>
              (regulars :+ k(s, t), virtuals)

            case (Some(s), Some(t)) if printVirtualEdges =>

              val numConcreteUses = (virtuals getOrElse ((s, t), 0)) + 1

              (regulars, virtuals + ((s, t) -> numConcreteUses))

            case _ => (regulars, virtuals)
          }
      }


    //(reg filterNot virt.contains, virt)
  }


  def print(): Unit = {
    writeln("digraph G{")
    writeln("rankdir=LR; ranksep=equally; compound=true")

    graph.content(graph.rootId).foreach(printNode)
    graph.nodesId.foreach{ nid => if(graph.container(nid).isEmpty) printNode(nid) }

    if(redOnly){
      val (reg, virt) = filterEdgeBasedOnVisibleNodes(concreteViolations)

      reg.foreach {
        case Isa(s, t) => printArc(ColorThickness.violation,isaStyle)((s, t))
        case Uses(s, t, _) => printArc(ColorThickness.violation,usesStyle)((s, t))
        case ParameterizedUses(s, t, _) => printArc(ColorThickness.violation,usesStyle)((s, t))
        case _ => ()
      }

      virt.foreach {
        case (e, numConcretes) =>
          printArc(ColorThickness.violation, virtualUse,
            Some(numConcretes.toString))(e)
      }
    }
    else {
      val (regularsIsa, virt0, virtualViolations0) =
        filterAllEdgeBasedOnVisibleNodesAndFlagViolations(graph.isaList map Isa.apply)

      regularsIsa.foreach {
        case (e, v) => printArc(violationStyle(v), isaStyle)(e)
      }

      val (reg, virt, virtualViolations) =
        filterAllEdgeBasedOnVisibleNodesAndFlagViolations(graph.usesList map Uses.apply, virt0, virtualViolations0)

      reg.foreach { case (e, v) => printArc(violationStyle(v), usesStyle)(e) }

      virt.foreach {
        case (e, numConcretes) =>
          printArc(violationStyle(virtualViolations contains e), virtualUse,
            Some(numConcretes.toString))(e)
      }
    }

    arcs.foreach(writeln)

    writeln("}")

    writer.close()
  }
}
