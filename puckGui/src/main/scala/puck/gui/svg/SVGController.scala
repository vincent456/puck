package puck.gui.svg

import java.io._

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.util.XMLResourceDescriptor
import org.w3c.dom.Element
import org.w3c.dom.svg.{SVGGElement, SVGDocument}
import puck.gui.svg.actions.{DefaultSwingService, SwingService}
import puck.{FilesHandlerDG2ASTControllerOps, StackListener, GraphStack}
import puck.graph._
import puck.graph.constraints.ConstraintsParser
import puck.graph.io._
import puck.gui.{PrintingOptionsControl, TextAreaLogger}
import puck.gui.explorer.NodeInfosPanel
import puck.util.PuckLog

import scala.concurrent.{ExecutionContext, Future}
import scala.swing.Publisher
import scala.util.{Failure, Success}



abstract class SVGController
(val graphUtils : GraphUtils,
 val dg2ast: DG2AST,
 val frame : SVGPanel,
 val graphStack : GraphStack,
 val printingOptionsControl: PrintingOptionsControl
)
  extends StackListener with Publisher {

  val swingService : SwingService = DefaultSwingService
  import swingService._

  graphStack registerAsStackListeners this
  import graphStack._

  def update(graphStack: GraphStack) : Unit  =
    displayGraph(graphStack.graph)

  lazy val console = frame.console
  implicit val logger = new TextAreaLogger(console.textArea, _ => true )

  val initialGraph : DependencyGraph = dg2ast.initialGraph


  def nodesByName : Map[String, NodeId] =
      dg2ast.nodesByName


  def parseConstraints(decouple : File) : Unit = try {
    val cm = ConstraintsParser(dg2ast.nodesByName, new FileReader(decouple))
    pushGraph(graph.newGraph(constraints = cm))
  } catch {
    case e : Exception => logger.writeln(e.getMessage)
  }


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

  def showNodeInfos(nodeId: NodeId): Unit = {
    frame.centerPane.setRightComponent(
    new NodeInfosPanel(this, graph, nodeId).peer)
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






  def displayGraph
  (graph: DependencyGraph, fail : Boolean = false): Unit =
    SVGController.documentFromGraph(graph, graphUtils,
      printingOptionsControl.printingOptions) {
      res =>
        val smsg : Option[String] = res match {
          case Success(0) => None
          case Success(n) =>

            if (fail)
              Some("An error that cannot be recovered occured during the production of the SVG file by Graphviz")
            else {
              val tmpDir = System.getProperty("java.io.tmpdir")
              val f = new File(tmpDir + File.separator + "graph.dot")

              DotPrinter.genDot(graph, graphUtils.dotHelper,
                printingOptionsControl.printingOptions,
                new FileWriter(f))

//              SVGController.this.visibility = VisibilitySet.topLevelVisible(graph)
//
//              displayGraph(graph, fail = true)

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




  def saveRecordOnFile(file : File) : Unit = {
    Recording.write(file.getAbsolutePath, nodesByName, graph)
  }

  def loadRecord(file : File) : Unit = {
    try load(Recording.load(file.getAbsolutePath, nodesByName))
    catch {
      case Recording.LoadError(msg, m) =>
        implicit val verbosity = (PuckLog.NoSpecialContext, PuckLog.Error)
        logger writeln ("Record loading error " + msg)
        logger writeln ("cannot bind loaded map " + m.toList.sortBy(_._1).mkString("\n"))
        logger writeln ("with " + nodesByName.toList.sortBy(_._1).mkString("\n"))
    }

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
    val ustr = (graph, u).shows
    graph.getNode(u.used).kind.kindType match {
      case TypeDecl =>
        console.appendText(s"Type uses $ustr selected")
        val tmus = graph.typeMemberUsesOf(u)
        if(tmus.isEmpty)
          console.appendText("No type member uses associated")
        else
          console.appendText(tmus.map{tmu => (graph, tmu).shows}.mkString("TM uses are :\n", "\n", "\n"))

      case InstanceValueDecl =>
        console.appendText(s"Type Member uses $ustr selected")

        val tus = graph.typeUsesOf(u)
        if(tus.isEmpty)
          console.appendText("No type uses associated")
        else
          console.appendText(tus.map{tu => (graph, tu).shows}.mkString("type uses are :\n", "\n", "\n"))

      case _ => console.appendText("unhandled kind of used node")
    }
  }
  





//  def abstractionChoices(id: NodeId): Seq[JMenuItem] =
//    graph.getNode(id) match {
//      case n: ConcreteNode => abstractionChoices(n)
//      case vn: VirtualNode => Seq.empty
//    }

  def workingDirectory : File
  def deleteOutDirAndapplyOnCode() : Unit
  def compareOutputGraph() : Unit



}




object SVGController {

  type  Builder  = SVGPanel => SVGController

  def builderFromFilesHander
  ( fh: FilesHandler,
    graphUtils : GraphUtils,
    dg2ast : DG2AST,
    graphStack: GraphStack,
    printingOptionsControl: PrintingOptionsControl) : Builder =
  ( frame : SVGPanel) =>
    new SVGController(graphUtils, dg2ast, frame,
      graphStack, printingOptionsControl)
    with FilesHandlerDG2ASTControllerOps {

      def graph = graphStack.graph
      val filesHandler = fh

      displayGraph(graph)

    }


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