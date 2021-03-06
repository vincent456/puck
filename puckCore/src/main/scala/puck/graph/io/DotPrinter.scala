/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph
package io


import java.io._

import puck.graph.constraints.ConstraintsMaps

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.sys.process.Process
import scala.util.Try

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

  var dotPath : Option[String] = None

  type DotProcessBuilder = scala.sys.process.ProcessBuilder
  // relies on dot directory being in the PATH variable
  def dotProcessBuilderFromFile(pathWithoutSuffix : String, outputFormat: DotOutputFormat) : DotProcessBuilder =
    Process(List(dotPath getOrElse "dot", "-T" + outputFormat, pathWithoutSuffix + ".dot"))

  def dotProcessBuilderFromFileToFile(pathWithoutSuffix : String, outputFormat: DotOutputFormat) : DotProcessBuilder =
    Process(List(dotPath getOrElse  "dot", "-O", "-T" + outputFormat, pathWithoutSuffix + ".dot"))
  // processBuilder #> new File( pathWithoutSuffix + "." + outputFormat)).!

  def dotProcessBuilderFromInputStream(input : InputStream, outputFormat: DotOutputFormat) : DotProcessBuilder=
    Process(List(dotPath getOrElse "dot", "-T" + outputFormat)) #< input


  def genDot
  ( graph : DependencyGraph,
    dotHelper: DotHelper,
    scm : Option[ConstraintsMaps],
    printingOptions: PrintingOptions,
    writer : OutputStreamWriter) : Unit =
    new DotPrinter(new BufferedWriter(writer), graph, dotHelper, scm, printingOptions).print()


  def genDotFile
  ( graph : DependencyGraph,
    dotHelper: DotHelper,
    scm : Option[ConstraintsMaps],
    printingOptions: PrintingOptions,
    pathWithoutSuffix : String) : Unit =
    genDot(graph, dotHelper, scm, printingOptions, new FileWriter(pathWithoutSuffix + ".dot"))


  def genImage
  (graph : DependencyGraph,
   dotHelper: DotHelper,
   scm : Option[ConstraintsMaps],
   printingOptions: PrintingOptions,
   outputFormat: DotOutputFormat,
   output : OutputStream)
  (finish : Try[Int] => Unit = {case _ => ()})
  (implicit executor: ExecutionContext) = {

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    Future {
      (dotProcessBuilderFromInputStream(pipedInput, outputFormat) #> output).!
    } onComplete finish

    genDot(graph, dotHelper, scm, printingOptions, writer = new OutputStreamWriter(pipedOutput))
  }

}
import VisibilitySet._
case class PrintingOptions
(visibility : VisibilitySet.T,
 printId : Boolean = false,
 printSignatures : Boolean = false,
 selectedUse : Option[NodeIdP] = None,
 printVirtualEdges : Boolean = false,
 printTypeUses : Boolean = true,
 printConcreteUsesPerVirtualEdges : Boolean = true,
 redOnly: Boolean = false)

import DotPrinter._
class DotPrinter
( writer: BufferedWriter,
  graph : DependencyGraph,
  helper : DotHelper,
  scm : Option[ConstraintsMaps],
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
        (graph, styp).shows.replaceAllLiterally("<", "&lt;")
          .replaceAllLiterally(">", "&gt;") + " "
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

  private val concreteViolations : Seq[DGEdge] = scm match {
    case None => Seq()
    case Some(cm) => (cm forbiddenDependencies graph) map transformParamOrDefToDecl distinct
  }


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
        case NameSpace => nodeSubGraphId(n.id.toString)
        case ValueDef | Parameter | TypeVariableKT | LocalValue =>
          dotId(graph.container_!(nid))
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

      arcs += (dotId(source) + " -> " + dotId(target) + " [ " +
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

  import puck.graph.NodeIdPOps

  val typeRelationShipLabel : NodeIdP => String =
    selectedUse match {
      case None => _ => ""
      case Some(selected) =>
        val labelMap : Map[NodeIdP, String]= graph.kindType(selected.used) match {
          case TypeDecl =>
            val init = Map(selected -> "TDecl")
            (graph typeMemberUsesOf selected).foldLeft(init){
              (map, tm) => map + (tm -> "TMember")
            }
          case InstanceValue
               | TypeConstructor =>
            val init = Map(selected -> "TMember")
            (graph typeUsesOf selected).foldLeft(init){
              (map, tm) => map + (tm -> "TDecl")
            }
//          case InstanceTypeDecl =>
//            sys.error("selection kind unhandle [TODO] - DotPrinter.typeRelationShipLabel")
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

  def decorate_name(n : DGNode, typeVariables : Seq[DGNode] = Seq()):String = {
    val sCter = graph.container(n.id)

    val tvsString =
      if(typeVariables.nonEmpty) typeVariables map (_.name) mkString("&lt;", ",","&gt;")
      else ""

    val name0 = helper.namePrefix(n) + name(n) + tvsString + idString(n.id)

    if (sCter.isDefined && concreteViolations.contains(Contains(sCter.get, n.id)))
      "<FONT COLOR=\"" + ColorThickness.violation.color + "\"><U>" + name0 + "</U></FONT>"
    else name0
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
        case InstanceValue if n.definition(g).isEmpty =>
          ("<I>", "</I>")
        case StableValue  =>
          ("<I>&lt;&lt;static&gt;&gt;</I>", "")
        case _ => ("","")
      }

      writeln(s"""<TR><TD PORT="${n.id}" HREF="${n.id}" ALIGN="LEFT" BORDER="0">"""+ itb +
      //writeln(s"""<TR><TD PORT="${n.id}" ID="${n.id}" ALIGN="LEFT" BORDER="0">"""+ itb +
        decorate_name(n) + sig + ite + "</TD></TR>")
    }

    val m = DependencyGraph.splitByKind (graph, graph.content(nid).toSeq filter visibility.isVisible)
    val fields = m.getOrElse("EnumConstant", Seq()) ++
      m.getOrElse("StaticField", Seq()) ++
      m.getOrElse("Field", Seq())

    val ctrs = m.getOrElse("Constructor", Seq())

    val mts = m.getOrElse("StaticMethod", Seq()) ++
      m.getOrElse("AbstractMethod", Seq()) ++
      m.getOrElse("Method", Seq())

    val innerClasses = m.getOrElse("Interface", Seq()) ++
      m.getOrElse("Class", Seq())

    val typeVariables = m.getOrElse("TypeVariable", Seq())



    writeln(s""" ${n.id} [ label = <<TABLE BGCOLOR="${helper.fillColor(n)}"> <TR> <TD PORT="${n.id}" HREF="${n.id}" BORDER="0"> <B>""" +
    //writeln(s""" ${n.id} [ label = <<TABLE BGCOLOR="${helper.fillColor(n)}"> <TR> <TD PORT="${n.id}" BORDER="0"> <B>""" +
      decorate_name(n, typeVariables map graph.getNode) +" </B></TD></TR>")

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
        case IsaEdge(s, t) => printArc(ColorThickness.violation,isaStyle)((s, t))
        case Uses(s, t) => printArc(ColorThickness.violation,usesStyle)((s, t))
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
        filterAllEdgeBasedOnVisibleNodesAndFlagViolations(graph.isaList map IsaEdge.apply)

      regularsIsa.foreach {
        case (e, v) => printArc(violationStyle(v), isaStyle)(e)
      }


      val usesList =
        if(printTypeUses) graph.usesList
        else graph.usesListExludingTypeUses

      val (reg, virt, virtualViolations) =
        filterAllEdgeBasedOnVisibleNodesAndFlagViolations(usesList map Uses.apply, virt0, virtualViolations0)

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
