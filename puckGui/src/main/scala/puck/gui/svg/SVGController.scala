package puck.gui.svg

import java.io._
import javax.swing.{SwingUtilities, JMenuItem}

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.util.XMLResourceDescriptor
import org.w3c.dom.Element
import org.w3c.dom.svg.{SVGGElement, SVGDocument}
import puck.graph._
import puck.graph.comparison.Mapping
import puck.graph.constraints.ConstraintsParser
import puck.graph.io._
import puck.graph.transformations.MileStone
import puck.gui.TextAreaLogger
import puck.gui.explorer.NodeInfosPanel
import puck.gui.svg.actions.{AddNodeAction, AbstractionAction}
import puck.util.PuckLog
import sbt.IO

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

import  VisibilitySet._
trait StackListener{
  def update(svgController: SVGController) : Unit
}

abstract class SVGController
( val graphUtils : GraphUtils,
  val dg2ast: DG2AST,
  val frame : SVGPanel,
  private var visibility : VisibilitySet.T,
  private var printId : Boolean,
  private var printSignatures : Boolean,
  private var printVirtualEdges : Boolean = true,
  private var printConcreteUsesPerVirtualEdges : Boolean = true,
  private var printRedOnly : Boolean = true,
  private var selectedEdgeForTypePrinting : Option[DGUses] = None) {

  implicit val executor : ExecutionContext

  val console = frame.console
  implicit val consoleLogger = new TextAreaLogger(console.textArea, _ => true )

  def nodesByName : Map[String, NodeId] =
      dg2ast.nodesByName


  private val undoStack = mutable.Stack[DependencyGraph]()
  private val redoStack = mutable.Stack[DependencyGraph]()

  private val stackListeners = mutable.ArrayBuffer[StackListener]()

  def registerAsStackListeners(l : StackListener) =
    stackListeners.append(l)

  def updateStackListeners() : Unit =
    stackListeners.foreach(_.update(this))


  def parseConstraints(decouple : File) : Unit = try {
    val cm = ConstraintsParser(dg2ast.nodesByName, new FileReader(decouple))
    pushGraph(graph.newGraph(constraints = cm))
  } catch {
    case e : Exception => consoleLogger.writeln(e.getMessage)
  }


  type Color = String

  def setSignatureVisible(b : Boolean): Unit = {
    if( b != printSignatures ){
      printSignatures = b
      displayGraph(graph)
    }
  }
  def setSelectedEdgeForTypePrinting(se: Option[DGUses]) : Unit = {
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
  def setConcreteUsesPerVirtualEdges(b : Boolean): Unit = {
    if( b != printConcreteUsesPerVirtualEdges ){
      printConcreteUsesPerVirtualEdges = b
      displayGraph(graph)
    }
  }

  def setRedEdgesOnly(b : Boolean): Unit = {
    if( b != printRedOnly ){
      printRedOnly = b
      displayGraph(graph)
    }
  }

  var selectedNodes0: List[(NodeId, Color, Element)] = List()
  def selectedNodes: List[(NodeId, Color, Element)] = selectedNodes0

  def getSelectedNode(nodeId: NodeId) : Option[(NodeId, Color, Element)]=
    selectedNodes.find( _._1 == nodeId)

  def isSelected(nodeId: NodeId) : Boolean =
    selectedNodes.exists( _._1 == nodeId)

  def removeSelectedNode(nodeId: NodeId) : Unit =
    selectedNodes0 = selectedNodes0.filter(_._1 != nodeId)

  def keepOnlySelectedNode(nodeId: NodeId) : List[(NodeId, Color, Element)] = {
    val (keep, others) = selectedNodes partition (_._1 == nodeId)
    selectedNodes0 = keep
    others
  }

  def showNodeInfos(nodeId: NodeId): Unit = {
    frame.centerPane.setRightComponent(
    new NodeInfosPanel(graph, nodeId){
      def onEdgeButtonClick( source : NodeId, target : NodeId) : Unit = ()
    }.peer)
    frame.centerPane.revalidate()
  }

  def hide(id : NodeId): Unit = {
    visibility = visibility.setVisibility(id, Hidden)
    setSubTreeVisibility(id, Hidden)
  }

  def focusExpand(id : NodeId, focus : Boolean, expand : Boolean) : Unit = {
    if(focus)
      visibility = VisibilitySet.allHidden(graph).
        setVisibility(graph.containerPath(id), Visible)

    if(expand)
      visibility =
        graph.content(id).foldLeft(visibility)(_.setVisibility(_, Visible))

    displayGraph(graph)
  }

  def focus(e : NodeIdP) : Unit = {
    val concretes = DGEdge.concreteEdgesFrom(graph, e)
    visibility = concretes.foldLeft(VisibilitySet.allHidden(graph)){
      case (set, DGEdge(_, source, target)) =>
        val s2 = set.setVisibility(graph.containerPath(source), Visible)
        s2.setVisibility(graph.containerPath(target), Visible)
    }
    displayGraph(graph)
  }

  private def setSubTreeVisibility(rootId : NodeId, v : Visibility): Unit ={
    val nodes = graph.subTree(rootId, includeRoot = false)
    visibility = visibility.setVisibility(nodes, v)
    displayGraph(graph)
  }

  def collapse(root: NodeId) : Unit =
    setSubTreeVisibility(root, Hidden)



  def expandAll(root: NodeId) : Unit =
    setSubTreeVisibility(root, Visible)

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
  def selectedEdge : Option[(NodeIdP, Color, SVGGElement)] = selectedEdge0

  def setEdgeSelected(dgEdge: NodeIdP, elt : SVGGElement, c : Color) = {
    selectedEdge0 = Some((dgEdge, c, elt))
    import ShowDG._
    console.displaySelection((graph, dgEdge).shows)
  }

  def resetEdgeSelected(): Unit = {
    selectedEdge0 = None
    console.displaySelection("")
  }

  

  def printingOptions =
    PrintingOptions(visibility, printId, printSignatures,
    selectedEdgeForTypePrinting,
    printVirtualEdges, printConcreteUsesPerVirtualEdges,
    printRedOnly)


  def displayGraph
  (graph: DependencyGraph): Unit =
    SVGController.documentFromGraph(graph, graphUtils.dotHelper, printingOptions)(
      msg => swingInvokeLater(() => console appendText msg)){
      case doc =>
        swingInvokeLater(() => frame.canvas.setDocument(doc))
    }


  def canUndo = undoStack.nonEmpty

  def undoAll() = {

    while(undoStack.nonEmpty)
      redoStack.push(undoStack.pop())

    displayGraph(graph)
    updateStackListeners()
  }

  def undo() = {
      val comments = graph.recording.commentsSinceLastMileStone
      redoStack.push(undoStack.pop())
      displayGraph(graph)
      ("Undo " +: comments).foreach(consoleLogger.writeln(_))
      updateStackListeners()
  }
  def canRedo = redoStack.nonEmpty

  def redo()={
    undoStack.push(redoStack.pop())
    displayGraph(graph)
    val comments = graph.recording.commentsSinceLastMileStone
    ("Redo " +: comments).foreach(consoleLogger.writeln(_))
    updateStackListeners()
  }

  def pushGraph(graph: DependencyGraph) = {
    undoStack.push(graph)
    redoStack.clear()
    displayGraph(graph)

    //console.displayWeight(Metrics.weight(graph, graph.nodeKindKnowledge.lightKind))

    updateStackListeners()
  }

  def graph =
    if(undoStack.nonEmpty) undoStack.head
    else dg2ast.initialGraph

  def saveRecordOnFile(file : File) : Unit = {
    Recording.write(file.getAbsolutePath, nodesByName, graph)
  }

  def loadRecord(file : File) : Unit = {
    try {
      val r = Recording.load(file.getAbsolutePath, nodesByName)
      pushGraph(r.reverse.foldLeft(graph){
        case (g, MileStone) =>
          undoStack.push(g)
          MileStone.redo(g)
        case (g, t) => t.redo(g)
      })
    }
    catch {
      case Recording.LoadError(msg, m) =>
        implicit val verbosity = (PuckLog.NoSpecialContext, PuckLog.Error)
        consoleLogger writeln ("Record loading error " + msg)
        consoleLogger writeln ("cannot bind loaded map " + m.toList.sortBy(_._1).mkString("\n"))
        consoleLogger writeln ("with " + nodesByName.toList.sortBy(_._1).mkString("\n"))
    }

  }

  import ShowDG._
  def printRecording() : Unit =
    graph.recording.reverseIterator.foreach(r => consoleLogger writeln (graph, r).shows )

  def printCode(nodeId: NodeId): Unit = {
    console.appendText("Code : ")
    console.appendText(dg2ast.code(graph, nodeId))
  }

  def printAbstractions() : Unit =
    consoleLogger writeln graph.abstractionsMap.content.mkString("\n\t")

  def printAbstractions(nodeId : NodeId) : Unit = {
    val absSet = graph.abstractions(nodeId)
    if(absSet.nonEmpty) {
      console.appendText(s"Abstractions of ${graph.getNode(nodeId)} :")
      console.appendText(absSet.mkString("\n"))
    }
    else
      console.appendText(s"${graph.getNode(nodeId)} has no abstractions.")
  }

  def printUseBindings(u : DGUses) : Unit = {
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

  def workingDirectory : File
  def deleteOutDirAndapplyOnCode() : Unit
  def compareOutputGraph() : Unit
  def swingInvokeLater (f : () => Unit ) : Unit

  pushGraph(dg2ast.initialGraph)
}




object SVGController {

  type  Builder  = SVGPanel => SVGController

  def builderFromFilesHander
  ( filesHandler: FilesHandler,
    opts : PrintingOptions,
    graphUtils : GraphUtils,
    dg2ast : DG2AST ) : Builder =
  ( frame : SVGPanel) =>  new SVGController(graphUtils, dg2ast, frame,
                opts.visibility, opts.printId, opts.printSignatures){
      //val filesHandler: FilesHandler = filesHandler0


      implicit val executor = scala.concurrent.ExecutionContext.Implicits.global

      def deleteOutDirAndapplyOnCode() : Unit = {
        console.appendText("Aplying recording on AST")
        dg2ast(graph)/*(new PuckFileLogger(_ => true, new File("/tmp/pucklog")))*/

        filesHandler.outDirectory.get match {
          case None => console.appendText("no output directory : cannot print code")
          case Some(d) =>
            console.appendText("Printing code")
            IO.delete(d)
            dg2ast.printCode(d)
        }

      }

      def compareOutputGraph() : Unit = {
        val outfh = filesHandler.fromOutDir
        console.appendText("Loading output graph from code")
        val outdg2ast = outfh.loadGraph(graphUtils.dG2ASTBuilder, null)
        console.appendText("Comparing graphs ...")

        val res = if(Mapping.equals(graph, outdg2ast.initialGraph)) "EQUAL"
        else "NOT equal"

        console.appendText(s"they are $res")
      }

      def workingDirectory : File = filesHandler.workingDirectory

      def swingInvokeLater (f : () => Unit ) : Unit =
        SwingUtilities.invokeLater(new Runnable {
          def run(): Unit = f()
        })
    }


  def documentFromStream(stream: InputStream): SVGDocument = {
    val parser: String = XMLResourceDescriptor.getXMLParserClassName
    val factory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(parser)
    factory.createSVGDocument("", stream)
  }

  def documentFromGraph
  ( graph: DependencyGraph,
    dotHelper : DotHelper,
    printingOptions: PrintingOptions)
  ( onDotConversionResult: String => Unit)
  ( onDocBuildingSuccess : PartialFunction[SVGDocument, Unit])
  ( implicit executor: ExecutionContext )= {

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)
    val fdoc = Future {
      SVGController.documentFromStream(pipedInput)
    }

    DotPrinter.genImage(graph, dotHelper, printingOptions, Svg, pipedOutput){
      res =>
      val msg = res match {
       case Success(0) => "Success"
        case Success(n) =>
          "An error occured during the production of the SVG file, " +
            "produced dot file can be found in out/graph.dot"
        case Failure(errMsg) =>
          "Image creation failure : " + errMsg

      }
      onDotConversionResult(msg)
    }

    fdoc.onSuccess(onDocBuildingSuccess)
  }
}