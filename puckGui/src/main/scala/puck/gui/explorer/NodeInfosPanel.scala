package puck.gui.explorer

import java.awt.Color

import puck.graph._
import puck.graph.io.VisibilitySet
import VisibilitySet._
import puck.gui._
import ShowDG._
import scala.swing._
import scala.swing.event.MouseClicked


class NodeInfosPanel
( publisher: Publisher,
  val graph : DependencyGraph,
  val nodeId : NodeId )
  extends SplitPane(Orientation.Horizontal) {

  def onEdgeButtonClick( source : NodeId, target : NodeId) : Unit =
    publisher publish
      GraphDisplayRequest("Graph with uses selected",
        graph, VisibilitySet.topLevelVisible(g).
          hideWithName(g, Seq("@primitive")).
          hideWithName(g, Seq("java")),
        sUse = Some(Uses(source, target)))

  implicit val g = graph
  val useDetails = new BoxPanel(Orientation.Vertical){
    minimumSize = new Dimension(100, 50)
  }

  def addUsesSet(title : String, usesSet : Set[Uses]) : Unit =
    if(usesSet.nonEmpty) {
      useDetails.contents += new Label(title)
      usesSet.foreach(e => useDetails.contents += new Label((graph, e).shows))
  }

  resizeWeight = 0.75

  val node = graph.getConcreteNode(nodeId)

  def mkStringWithNames(nodes : Iterable[NodeId]): String ={
    nodes.map{ nid =>
      val node = graph.getNode(nid)
      (graph, node).shows(nodeNameTypCord)
    }.mkString("\n", "\n", "\n")
  }

  val typeWeight =
    node.kind.kindType match {
      case TypeDecl =>
        val m = Metrics.typeWeight(graph, nodeId)
        s"Type Weight = ${m.toString}\n"

      case _ => ""
    }

  leftComponent = new BoxPanel(Orientation.Vertical) {

    background = Color.white

    contents += new Label(node.kind + " : " +
      (graph, node).shows(nodeNameTypCord))
    val prov = Metrics.providers(graph, node.id)
    val cl = Metrics.clients(graph, node.id)
    val internals = Metrics.internalDependencies(graph, node.id).size
    val outgoings = Metrics.outgoingDependencies(graph, node.id).size
    val incomings = Metrics.incomingDependencies(graph, node.id).size
    val coupling = Metrics.coupling(graph, node.id)
    val cohesion = Metrics.cohesion(graph, node.id)

    contents +=
      new Label(s"Internal dependencies : $internals \n") {
        tooltip = "Number of uses edges with both extremities contained by this node"
      }.leftGlued

    contents += new Label(s"Outgoing dependencies : $outgoings \n"){
      tooltip = "Number of uses edges for which this node contains the user but not the used"
    }.leftGlued

    contents += new Label(s"Incoming dependencies : $incomings \n"){
      tooltip = "Number of uses edges for which this node contains the used but not the user"
    }.leftGlued

    contents += new Label(typeWeight){
      tooltip = "Number of strongly connected component divided by the number of children of this type node"
    }.leftGlued

    contents += new TextArea("Subtypes : " +
      (if(graph.directSubTypes(node.id).isEmpty) "none\n"
      else mkStringWithNames(graph.directSubTypes(node.id)))){
      tooltip = "Number of direct subtype"
      editable = false
    }.leftGlued

    contents += new TextArea( "Super types :" +
      (if(graph.directSuperTypes(node.id).isEmpty) "none\n"
      else mkStringWithNames(graph.directSuperTypes(node.id)))){
      tooltip = "Number of direct super types"
      editable = false
    }.leftGlued

    contents += new TextArea(  "Providers : " +
      (if (prov.isEmpty) "none\n"
      else mkStringWithNames(prov))){
      tooltip = "Nodes of the same kinds which contain a node used by a node contained by this one"
      editable = false
    }.leftGlued

    contents += new TextArea(  "Clients : " +
      (if (cl.isEmpty) "none\n"
      else mkStringWithNames(cl)) ){
        tooltip = "Nodes of the same kinds which contain a node using a node contained by this one"
      editable = false
      }.leftGlued

    contents += new BoxPanel(Orientation.Horizontal){
      contents += new Label(f"Coupling = $coupling%.2f"){
        tooltip = "1 - |Clients U Providers| / |Internal + Outgoing + Incoming dependencies |"
      }
      contents += Swing.HGlue
      contents += new Label(f"Cohesion :  $cohesion%.2f"){
        tooltip = "|Internal dependencies| / |Internal + Outgoing + Incoming dependencies|"
      }
    }


    def usesLabelBox(userId : NodeId, usedId : NodeId) : Component = {
      val sideUses = graph.typeMemberUsesOf(userId, usedId)
      val primaryUses = graph.typeUsesOf(userId, usedId)

      def tag = (sideUses.isEmpty, primaryUses.isEmpty) match {
        case (true, true) => ""
        case (false, true) => "(dominant use)"
        case (true, false) => "(dominated use)"
        case _ => "(both dominant and dominated)"
      }


      new BoxPanel(Orientation.Horizontal){

        contents += Button("<o>") {
          onEdgeButtonClick(userId, usedId)
        }

        contents += new Label(graph.fullName(userId) + " " + tag) {

          minimumSize = new Dimension(this.size.width, 30)

          listenTo(mouse.clicks)
          reactions += {
            case MouseClicked(_, _, _, _, _) =>

              useDetails.contents.clear()
              useDetails.contents +=
                new Label((graph, Uses(userId, usedId)).shows)

              addUsesSet("Dominant Uses :", primaryUses)
              addUsesSet("Dominated Uses :", sideUses)

              useDetails.revalidate()
          }
        }

        contents += Swing.HGlue
      }
    }

    contents +=  new Label("used by:").leftGlued


    contents += new BoxPanel(Orientation.Vertical) {

      graph.usersOf(node.id).foreach {
        userId =>
        contents += usesLabelBox(graph.declarationOf(userId), node.id)
      }

    }
    graph.definitionOf(node.id) foreach {
      defId =>
        contents +=  new Label("uses :").leftGlued
        contents += new BoxPanel(Orientation.Vertical) {
          graph.usedBy(defId).foreach {
            contents += usesLabelBox(_, defId)
          }
        }
    }

  }

  rightComponent = useDetails
}
