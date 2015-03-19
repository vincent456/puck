package puck.gui.svg

import java.io.{InputStream, PipedInputStream, PipedOutputStream}
import java.util
import javax.swing.JMenuItem

import org.apache.batik.dom.svg.SAXSVGDocumentFactory
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.util.XMLResourceDescriptor
import org.w3c.dom.Element
import org.w3c.dom.svg.{SVGGElement, SVGDocument}
import puck.graph._
import puck.graph.constraints.{RedirectionPolicy, SupertypeAbstraction, DelegationAbstraction}
import puck.graph.io.{PrintingOptions, Svg, VisibilitySet}
import puck.gui.PuckControl
import puck.gui.svg.SVGFrame.SVGConsole

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future




class SVGController private
( val genController : PuckControl,
  val svgCanvas : JSVGCanvas,
  val console : SVGConsole,
  var visibility : VisibilitySet,
  var printId : Boolean,
  var printSignatures : Boolean) {

  def transfoRules = genController.filesHandler.transformationRules

  val graphStack = mutable.Stack[DependencyGraph]()

  type Color = String

  var nodeSelected: Option[(NodeId, Color, Element)] = None

  /*
   Code mostly accessed from java code thus this "java-like" design with getters
   */
  def nodeIsSelected: Boolean = nodeSelected.nonEmpty

  def getNodeSelected: Int = nodeSelected.get._1

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

  def pushGraph(graph: DependencyGraph) = {
    graphStack.push(graph)
    displayGraph(graph)
  }


  def safePopGraph() = {
    if(graphStack.nonEmpty) {
      graphStack.pop()
      displayGraph(getGraph)
    }
  }

  def getGraph = graphStack.head

  def abstractionChoices(n: ConcreteNode): java.util.List[JMenuItem] = {
    val l = for {
      p <- n.kind.abstractionPolicies
      k <- n.kind.abstractKinds(p)
    } yield {
        new JMenuItem(AbstractionAction(n, p, k, this))
      }

    seqAsJavaList(l)
  }

  def abstractionChoices(id: NodeId): java.util.List[JMenuItem] =
    getGraph.getNode(id) match {
      case n: ConcreteNode => abstractionChoices(n)
      case vn: VirtualNode => new util.ArrayList[JMenuItem]()
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
    c.graphStack.push(g)
    c
  }

  def documentFromStream(stream: InputStream): SVGDocument = {
    val parser: String = XMLResourceDescriptor.getXMLParserClassName
    val factory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(parser)
    factory.createSVGDocument("", stream)
  }

}