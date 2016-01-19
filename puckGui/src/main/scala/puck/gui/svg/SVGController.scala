package puck.gui.svg

import java.io._
import javax.swing.JPopupMenu

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.util.XMLResourceDescriptor
import org.w3c.dom.Element
import org.w3c.dom.svg.{SVGGElement, SVGDocument}
import puck.gui.svg.actions.{DefaultSwingService, SwingService}
import puck.{StackListener, GraphStack}
import puck.graph._
import puck.graph.constraints.ConstraintsParser
import puck.graph.io._
import puck.gui._
import puck.gui.explorer.NodeInfosPanel
import puck.util.PuckLog

import scala.concurrent.{ExecutionContext, Future}
import scala.swing.Publisher
import scala.util.{Failure, Success}



class SVGController
(val genControl : PuckControl,
 val frame : SVGPanel)
  extends StackListener with Publisher {

  this listenTo genControl.printingOptionsControl
  genControl listenTo this

  reactions += {
    case PrintingOptionsUpdate =>
      this.displayGraph()
  }

  val graphUtils : GraphUtils = genControl.graphUtils
  def dg2ast: DG2AST = genControl.dg2ast
  val graphStack : GraphStack = genControl.graphStack
  val printingOptionsControl: PrintingOptionsControl =
    genControl.printingOptionsControl

  val swingService : SwingService = DefaultSwingService
  import swingService._

  graphStack registerAsStackListeners this
  import graphStack.graph

  def update(graphStack: GraphStack) : Unit  =
    displayGraph()

  lazy val console = frame.console
  implicit val logger = new TextAreaLogger(console.textArea, _ => true )


  type Color = String

  def selectedNodes: List[NodeId] = selectedSVGNodes map (_._1)

  def selectedEdge : Option[NodeIdP] = selectedSVGEdge map (_._1)

  var selectedNodes0: List[(NodeId, Color, Element)] = List()
  def selectedSVGNodes: List[(NodeId, Color, Element)] = selectedNodes0

  def getSelectedNode(nodeId: NodeId) : Option[(NodeId, Color, Element)]=
    selectedSVGNodes.find( _._1 == nodeId)

  def isSelected(nodeId: NodeId) : Boolean =
    selectedSVGNodes.exists( _._1 == nodeId)

  def removeSelectedNode(nodeId: NodeId) : Unit =
    selectedNodes0 = selectedNodes0.filter(_._1 != nodeId)

  def keepOnlySelectedNode(nodeId: NodeId) : List[(NodeId, Color, Element)] = {
    val (keep, others) = selectedSVGNodes partition (_._1 == nodeId)
    selectedNodes0 = keep
    others
  }

  val edgeMenuBuilder : NodeIdP => JPopupMenu = {
    e =>
       new EdgeMenu(this, e,
        printingOptionsControl,
        graphStack.graph,
        graphUtils )
  }

  def showNodeInfos(nodeId: NodeId): Unit = {
    frame.centerPane.setRightComponent(
    new NodeInfosPanel(this, graph, nodeId, edgeMenuBuilder).peer)
    frame.centerPane.revalidate()
  }

  val defaultColor = "black"

  def addNodeToSelection(id: NodeId, elt: Element): Unit = {
    val color =
      if(elt.getAttribute("fill").nonEmpty)
        elt.getAttribute("fill")
      else defaultColor
    selectedNodes0 :+= ((id, color, elt))
    val nodes = selectedNodes0 map {
      case (nid, _, _) => graph.getNode(nid)
    }
    console.displaySelection(nodes.mkString(", ") )
  }

  def resetSelectedNodes(): Unit = {
    selectedNodes0 = List()
    console.displaySelection("")
  }

  var selectedEdge0 : Option[(NodeIdP, Color, SVGGElement)] = None
  def selectedSVGEdge : Option[(NodeIdP, Color, SVGGElement)] = selectedEdge0


  def setEdgeSelected(dgEdge: NodeIdP, elt : SVGGElement, c : Color) = {
    selectedEdge0 = Some((dgEdge, c, elt))
    import ShowDG._
    console.displaySelection((graph, dgEdge).shows)
  }

  def resetEdgeSelected(): Unit = {
    selectedEdge0 = None
    console.displaySelection("")
  }

  def displayGraph(recCall : Boolean = false): Unit =
    SVGController.documentFromGraph(graphStack.graph, graphUtils,
      printingOptionsControl.printingOptions) {
      res =>
        val smsg : Option[String] = res match {
          case Success(0) => None
          case Success(n) =>

            if (recCall)
              Some("An error that cannot be recovered occured during the production of the SVG file by Graphviz")
            else {
              val tmpDir = System.getProperty("java.io.tmpdir")
              val f = new File(tmpDir + File.separator + "graph.dot")

              DotPrinter.genDot(graph, graphUtils.dotHelper,
                printingOptionsControl.printingOptions,
                new FileWriter(f))

              Some("error during SVG production dot can be found at " + f.getAbsolutePath /*+
                "\nretry with top level package visibility only"*/)

          }

          case Failure(errMsg) =>
            Some("Image creation failure : " + errMsg)

        }
        smsg foreach (msg => swingInvokeLater(() => console appendText msg))
    }{
      case doc =>
        swingInvokeLater(() => frame.canvas.setDocument(doc))
    }


  import ShowDG._
  def printRecording() : Unit =
    graph.recording.reverseIterator.foreach(r => logger writeln (graph, r).shows )

  def printCode(nodeId: NodeId): Unit = {
    console.appendText("Code : ")
    console.appendText(dg2ast.code(graph, nodeId))
  }

  def printAbstractions() : Unit =
    logger writeln graph.abstractionsMap.content.mkString("\n\t")

  def printAbstractions(nodeId : NodeId) : Unit = {
    val absSet = graph.abstractions(nodeId)
    if(absSet.nonEmpty) {
      console.appendText(s"Abstractions of ${graph.getNode(nodeId)} :")
      console.appendText(absSet.mkString("\n"))
    }
    else
      console.appendText(s"${graph.getNode(nodeId)} has no abstractions.")
  }

  def printUseBindings(u : Uses) : Unit = {
    def print(u : Uses) : Unit = {
      val ustr = (graph, u).shows
      graph.getNode(u.used).kind.kindType match {
        case TypeDecl =>
          console.appendText(s"Type uses $ustr selected")
          val tmus = graph.typeMemberUsesOf(u)
          if (tmus.isEmpty)
            console.appendText("No type member uses associated")
          else
            console.appendText(tmus.map { tmu => (graph, tmu).shows }.mkString("TM uses are :\n", "\n", "\n"))

        case InstanceValueDecl =>
          console.appendText(s"Type Member uses $ustr selected")

          val tus = graph.typeUsesOf(u)
          if (tus.isEmpty)
            console.appendText("No type uses associated")
          else
            console.appendText(tus.map { tu => (graph, tu).shows }.mkString("type uses are :\n", "\n", "\n"))

        case _ => console.appendText("unhandled kind of used node")
      }
    }
    print(u)
    graph.definitionOf(u.user).foreach {
      userDef => print(graph.getUsesEdge(userDef, u.used).get)
    }
  }


  displayGraph()



}




object SVGController {

  type  Builder  = SVGPanel => SVGController

  def documentFromStream(stream: InputStream): SVGDocument = {
    val parser: String = XMLResourceDescriptor.getXMLParserClassName
    val factory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(parser)
    factory.createSVGDocument("", stream)
  }


  def documentFromGraphErrorMsgGen(consumer : String => Unit) : scala.util.Try[Int] => Unit ={
        case Success(0) => ()
        case Success(n) =>
          consumer("An error that cannot be recovered occured during the production of the SVG file by Graphviz")
        case Failure(errMsg) =>
          consumer("Image creation failure : " + errMsg)
  }



  def documentFromGraph
  ( graph: DependencyGraph,
    graphUtils : GraphUtils,
    printingOptions: PrintingOptions,
    fail : Boolean = false)
  ( onDotConversionResult: scala.util.Try[Int] => Unit)
  ( onDocBuildingSuccess : PartialFunction[SVGDocument, Unit])
  ( implicit executor: ExecutionContext ) : Unit = {

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)
    val fdoc = Future {
      SVGController.documentFromStream(pipedInput)
    }

    DotPrinter.genImage(graph, graphUtils.dotHelper, printingOptions, Svg,
      pipedOutput)(onDotConversionResult)


    fdoc.onSuccess(onDocBuildingSuccess)
  }
}