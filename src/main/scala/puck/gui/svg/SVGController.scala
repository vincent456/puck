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

  var nodeSelected: Option[NodeId] = None
  var nodeColor: String = ""
  var domElement: Element = _

  /*
   Code mostly accessed from java code thus this "java-like" design with getters
   */
  def nodeIsSelected: Boolean = nodeSelected.nonEmpty

  def getNodeSelected: Int = nodeSelected.get

  def getDomElement: Element = domElement

  def getColor: String = nodeColor

  def setNodeSelected(id: NodeId, elt: Element): Unit = {
    nodeSelected = Some(id)
    domElement = elt
    nodeColor = elt.getAttribute("fill")
    console.selectedNode(getGraph.getNode(id)+"")
  }

  def resetNodeSelected(): Unit = {
    nodeSelected = None
    domElement = null
    console.selectedNode("")
  }

  var edgeSelected : Option[(NodeId, NodeId)] = None
  var edgeColor : String = ""
  var edgeDomElement : SVGGElement = _

  def edgeIsSelected : Boolean = edgeSelected.nonEmpty

  //Java accessor
  def usesKind = Uses
  def isaKind = Isa
  
  

  def pushGraph(graph: DependencyGraph) = {
    graphStack.push(graph)
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


  def popGraph() = graphStack.pop()

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