package puck.gui.svg

import java.io.{InputStream, PipedInputStream, PipedOutputStream}
import java.util
import javax.swing.{AbstractAction, JMenuItem}

import org.apache.batik.dom.svg.SAXSVGDocumentFactory
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.util.XMLResourceDescriptor
import org.w3c.dom.Element
import org.w3c.dom.svg.{SVGGElement, SVGDocument}
import puck.graph._
import puck.graph.constraints.{RedirectionPolicy, SupertypeAbstraction, DelegationAbstraction}
import puck.graph.io._
import puck.gui.PuckControl
import puck.gui.svg.SVGFrame.SVGConsole
import puck.gui.svg.actions.{AddNodeAction, AbstractionAction}

import scala.collection.JavaConversions._
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
  private val visibility : VisibilitySet,
  private var printId : Boolean,
  private var printSignatures : Boolean) {

  def transfoRules = genController.filesHandler.transformationRules

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
      displayGraph(getGraph)
    }
  }

  def setIdVisible(b : Boolean): Unit = {
    if( b != printId ){
      printId = b
      displayGraph(getGraph)
    }
  }

  var nodeSelected: Option[(NodeId, Color, Element)] = None

  def hide(id : NodeId): Unit = {
    visibility.setVisibility(id, Hidden)
    displayGraph(getGraph)
  }

  private def setSubTreeVisibility(rootId : NodeId, v : Visibility): Unit ={
    val nodes = getGraph.subTree(rootId, includeRoot = false)
    visibility.setVisibility(nodes, v)
    displayGraph(getGraph)
  }

  def collapse(root: NodeId) : Unit =
    setSubTreeVisibility(root, Hidden)

  def expand(root: NodeId) : Unit =
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
    console.displaySelection(getGraph.getNode(id)+"")
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
    console.displaySelection(showDG[DGEdge](getGraph).shows(dgEdge))
  }

  def resetEdgeSelected(): Unit = {
    edgeSelected = None
    console.displaySelection("")
  }

  //Java accessor
  def usesKind = Uses
  def isaKind = Isa

  def delegatePolicy : RedirectionPolicy = DelegationAbstraction
  def supertypePolicy : RedirectionPolicy  = SupertypeAbstraction
  

  def displayGraph(graph: DependencyGraph) = {


    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)
    val fdoc = Future {
      SVGController.documentFromStream(pipedInput)
    }

    val opts = PrintingOptions(visibility, printId, printSignatures)
    genController.filesHandler.makeImage(graph, opts, Some(pipedOutput), Svg){
      case _ => ()
    }

    fdoc.onSuccess { case doc => svgCanvas.setDocument(doc) }
  }

  def canUndo = undoStack.nonEmpty
  def undo() = {
      redoStack.push(undoStack.pop())
      displayGraph(getGraph)
      updateStackListeners()
  }
  def canRedo = redoStack.nonEmpty
  def redo()={
    undoStack.push(redoStack.pop())
    displayGraph(getGraph)
    updateStackListeners()
  }
  def pushGraph(graph: DependencyGraph) = {
    undoStack.push(graph)
    redoStack.clear()
    displayGraph(getGraph)
    updateStackListeners()
  }


  

  def getGraph = undoStack.head

  def abstractionChoices(n: ConcreteNode): Seq[JMenuItem] =
    for {
      p <- n.kind.abstractionPolicies
      k <- n.kind.abstractKinds(p)
    } yield {
        new JMenuItem(new AbstractionAction(n, p, k, this))
    }


  def abstractionChoices(id: NodeId): Seq[JMenuItem] =
    getGraph.getNode(id) match {
      case n: ConcreteNode => abstractionChoices(n)
      case vn: VirtualNode => new util.ArrayList[JMenuItem]()
    }

  def childChoices(n : ConcreteNode) : Seq[JMenuItem] = {
    val ks = getGraph.nodeKinds.filter(n.kind.canContain)
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