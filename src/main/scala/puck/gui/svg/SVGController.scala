package puck.gui.svg

import java.io.{File, InputStream, PipedInputStream, PipedOutputStream}
import javax.swing.JMenuItem

import org.apache.batik.dom.svg.SAXSVGDocumentFactory
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.util.XMLResourceDescriptor
import org.w3c.dom.Element
import org.w3c.dom.svg.{SVGGElement, SVGDocument}
import puck.graph._
import puck.graph.constraints.search.SolverBuilder
import puck.graph.io._
import puck.graph.transformations.{MileStone, Recording}
import puck.gui.PuckControl
import puck.gui.svg.actions.{AddNodeAction, AbstractionAction}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future



trait StackListener{
  def update(svgController: SVGController) : Unit
}

class SVGController private
( val genController : PuckControl,
  val svgCanvas : JSVGCanvas,
  val console : SVGConsole,
  val solverBuilder : SolverBuilder,
  private val visibility : VisibilitySet,
  private var printId : Boolean,
  private var printSignatures : Boolean,
  private var printVirtualEdges : Boolean = false,
  private var printRedOnly : Boolean = false,
  private var selectedEdgeForTypePrinting : Option[DGEdge] = None) {

  def transfoRules = genController.filesHandler.transformationRules

  def nodesByName : Map[String, NodeId] =
    genController.filesHandler.dg2ast.nodesByName


  private val undoStack = mutable.Stack[DependencyGraph]()
  private val redoStack = mutable.Stack[DependencyGraph]()

  private val stackListeners = mutable.ArrayBuffer[StackListener]()

  def registerAsStackListeners(l : StackListener) =
    stackListeners.append(l)

  def updateStackListeners() : Unit =
    stackListeners.foreach(_.update(this))


  type Color = String

  def setSignatureVisible(b : Boolean): Unit = {
    if( b != printSignatures ){
      printSignatures = b
      displayGraph(graph)
    }
  }
  def setSelectedEdgeForTypePrinting(se: Option[DGEdge]) : Unit = {
    if( se != selectedEdgeForTypePrinting ){
      selectedEdgeForTypePrinting = se
      displayGraph(graph)
    }
  }

  def setIdVisible(b : Boolean): Unit = {
    if( b != printId ){
      printId = b
      displayGraph(graph)
    }
  }

  def setVirtualEdgesVisible(b : Boolean): Unit = {
    if( b != printVirtualEdges ){
      printVirtualEdges = b
      displayGraph(graph)
    }
  }

  def setRedEdgesOnly(b : Boolean): Unit = {
    if( b != printRedOnly ){
      printRedOnly = b
      displayGraph(graph)
    }
  }

  var nodeSelected: Option[(NodeId, Color, Element)] = None

  def hide(id : NodeId): Unit = {
    visibility.setVisibility(id, Hidden)
    setSubTreeVisibility(id, Hidden)
  }

  private def setSubTreeVisibility(rootId : NodeId, v : Visibility): Unit ={
    val nodes = graph.subTree(rootId, includeRoot = false)
    visibility.setVisibility(nodes, v)
    displayGraph(graph)
  }

  def collapse(root: NodeId) : Unit =
    setSubTreeVisibility(root, Hidden)

  def expand(root: NodeId) : Unit = {
      graph.content(root).foreach(visibility.setVisibility(_, Visible))
      displayGraph(graph)
  }

  def expandAll(root: NodeId) : Unit =
    setSubTreeVisibility(root, Visible)

  /*
   Code mostly accessed from java code thus this "java-like" design with getters
   */
  def nodeIsSelected: Boolean = nodeSelected.nonEmpty

  def getIdNodeSelected: Int = nodeSelected.get._1

  def getNodeDomElement: Element = nodeSelected.get._3

  def getNodeColor: String = nodeSelected.get._2

  def setNodeSelected(id: NodeId, elt: Element): Unit = {
    nodeSelected = Some((id, elt.getAttribute("fill"), elt))
    console.displaySelection(graph.getNode(id)+"")
  }

  def resetNodeSelected(): Unit = {
    nodeSelected = None
    console.displaySelection("")
  }

  var edgeSelected : Option[(DGEdge, Color, SVGGElement)] = None

  def edgeIsSelected : Boolean = edgeSelected.nonEmpty

  def getEdgeSelected : DGEdge = edgeSelected.get._1
  def getEdgeDomElement : SVGGElement = edgeSelected.get._3
  def getEdgeColor : Color = edgeSelected.get._2

  def setEdgeSelected(dgEdge: DGEdge, elt : SVGGElement, c : Color) = {
    edgeSelected = Some((dgEdge, c, elt))

    import ShowDG._
    console.displaySelection(showDG[DGEdge](graph).shows(dgEdge))
  }

  def resetEdgeSelected(): Unit = {
    edgeSelected = None
    console.displaySelection("")
  }

  //Java accessor
  def usesKind = Uses
  def isaKind = Isa


  def displayGraph(graph: DependencyGraph) = {

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)
    val fdoc = Future {
      SVGController.documentFromStream(pipedInput)
    }

    val opts = PrintingOptions(visibility, printId, printSignatures, selectedEdgeForTypePrinting, printVirtualEdges, printRedOnly)
    genController.filesHandler.makeImage(graph, opts, Some(pipedOutput), Svg){
      case _ => ()
    }

    fdoc.onSuccess { case doc => svgCanvas.setDocument(doc) }
  }

  def canUndo = undoStack.nonEmpty

  def undoAll() = {
    while(undoStack.nonEmpty)
      redoStack.push(undoStack.pop())
    displayGraph(graph)
    updateStackListeners()
  }

  def undo() = {
      redoStack.push(undoStack.pop())
      displayGraph(graph)
      updateStackListeners()
  }
  def canRedo = redoStack.nonEmpty
  def redo()={
    undoStack.push(redoStack.pop())
    displayGraph(graph)
    updateStackListeners()
  }
  def pushGraph(graph: DependencyGraph) = {
    undoStack.push(graph)
    redoStack.clear()
    displayGraph(graph)
    updateStackListeners()
  }

  def graph = undoStack.head

  def saveRecordOnFile(file : File) : Unit = {
    Recording.write(file.getAbsolutePath, nodesByName, graph)
  }

  def loadRecord(file : File) : Unit = {
    val r = Recording.load(file.getAbsolutePath, nodesByName)

    pushGraph(r.reverse.foldLeft(graph){
      case (g, MileStone) =>
        undoStack.push(g)
        MileStone.redo(g)
      case (g, t) => t.redo(g)
    })
  }


  def applyOnCode() : Unit = {
    genController.applyOnCode((graph, graph.recording))
  }

  def abstractionChoices(n: ConcreteNode): Seq[JMenuItem] =
    n.kind.abstractionChoices.map { case (k, p) =>
      new JMenuItem(new AbstractionAction(n, p, k, this))
    }

  def abstractionChoices(id: NodeId): Seq[JMenuItem] =
    graph.getNode(id) match {
      case n: ConcreteNode => abstractionChoices(n)
      case vn: VirtualNode => Seq.empty
    }

  def childChoices(n : ConcreteNode) : Seq[JMenuItem] = {
    val ks = graph.nodeKinds.filter(n.kind.canContain)
    ks map {k => new JMenuItem(new AddNodeAction(n, this, k))}
  }
}


object SVGController {

  def apply(genController : PuckControl,
            g : DependencyGraph,
            opts : PrintingOptions,
            svgCanvas : JSVGCanvas,
            console : SVGConsole): SVGController ={
    val c = new SVGController(genController, svgCanvas, console,
                genController.filesHandler.solverBuilder,
                opts.visibility, opts.printId, opts.printSignatures)

    c.undoStack.push(g)
    c
  }

  def documentFromStream(stream: InputStream): SVGDocument = {
    val parser: String = XMLResourceDescriptor.getXMLParserClassName
    val factory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(parser)
    factory.createSVGDocument("", stream)
  }

}