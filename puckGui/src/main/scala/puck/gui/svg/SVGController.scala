package puck.gui.svg

import java.io._

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.util.XMLResourceDescriptor
import org.w3c.dom.Element
import org.w3c.dom.svg.{SVGGElement, SVGDocument}
import puck.gui.svg.actions.SwingGraphController
import puck.{StackListener, GraphStack}
import puck.graph._
import puck.graph.comparison.Mapping
import puck.graph.constraints.ConstraintsParser
import puck.graph.io._
import puck.graph.transformations.MileStone
import puck.gui.TextAreaLogger
import puck.gui.explorer.NodeInfosPanel
import puck.util.PuckLog
import sbt.IO

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

import  VisibilitySet._

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
  private var selectedEdgeForTypePrinting : Option[Uses] = None)
  extends SwingGraphController with StackListener {

  this registerAsStackListeners this

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

  def setSignatureVisible(b : Boolean): Unit = {
    if( b != printSignatures ){
      printSignatures = b
      displayGraph(graph)
    }
  }
  def setSelectedEdgeForTypePrinting(se: Option[Uses]) : Unit = {
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
    new NodeInfosPanel(graph, nodeId){
      def onEdgeButtonClick( source : NodeId, target : NodeId) : Unit = ()
    }.peer)
    frame.centerPane.revalidate()
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

  private def setSubTreeVisibility(rootId : NodeId, v : Visibility, includeRoot : Boolean): Unit ={
    val nodes = graph.subTree(rootId, includeRoot)
    visibility = visibility.setVisibility(nodes, v)
    displayGraph(graph)
  }

  def hide(root : NodeId): Unit =
    setSubTreeVisibility(root, Hidden, includeRoot = true)

  def collapse(root: NodeId) : Unit =
    setSubTreeVisibility(root, Hidden, includeRoot = false)

  def expandAll(root: NodeId) : Unit =
    setSubTreeVisibility(root, Visible, includeRoot = true)

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



  def printingOptions =
    PrintingOptions(visibility, printId, printSignatures,
    selectedEdgeForTypePrinting,
    printVirtualEdges, printConcreteUsesPerVirtualEdges,
    printRedOnly)


  def displayGraph
  (graph: DependencyGraph, fail : Boolean = false): Unit =
    SVGController.documentFromGraph(graph, graphUtils, printingOptions) {
      res =>
        val smsg : Option[String] = res match {
          case Success(0) => None
          case Success(n) =>

            if (fail)
              Some("An error that cannot be recovered occured during the production of the SVG file by Graphviz")
            else {
              val tmpDir = System.getProperty("java.io.tmpdir")
              val f = new File(tmpDir + File.separator + "graph.dot")

              DotPrinter.genDot(graph, graphUtils.dotHelper, printingOptions, new FileWriter(f))

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
    try {
      val r = Recording.load(file.getAbsolutePath, nodesByName)

//      val fw = new FileWriter("/tmp/prettyRecord")
//      import ShowDG._
//      graph.recording.reverseIterator.foreach{ t =>
//        fw.write((graph, t).shows + "\n")
//      }


      pushGraph(r.reverse.foldLeft(graph){
        case (g, MileStone) =>
          undoStack.push(g)
//          fw.write((g, MileStone).shows + "\n")
          MileStone.redo(g)
        case (g, t) =>
//          fw.write((g, t).shows + "\n")
          t.redo(g)
      })
//      fw.close()
    }
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
  ( filesHandler: FilesHandler,
    opts : PrintingOptions,
    graphUtils : GraphUtils,
    dg2ast : DG2AST ) : Builder =
  ( frame : SVGPanel) =>  new SVGController(graphUtils, dg2ast, frame,
                opts.visibility, opts.printId, opts.printSignatures){
      //val filesHandler: FilesHandler = filesHandler0

      pushGraph(dg2ast.initialGraph)

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
        val outdg2ast = outfh.loadGraph()
        console.appendText("Comparing graphs ...")

        val res = if(Mapping.equals(graph, outdg2ast.initialGraph)) "EQUAL"
        else "NOT equal"

        console.appendText(s"they are $res")
      }

      def workingDirectory : File = filesHandler.workingDirectory

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