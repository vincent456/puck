package puck.gui.explorer

import java.awt.Color
import javax.swing.{JTree, JPopupMenu}

import puck.graph._
import ShowDG._
import scala.swing._
import scala.swing.event.MouseClicked
import puck.gui._


class GraphTreePane(treeIcons : DGTreeIcons,
                    graph : DependencyGraph,
                    focus : Set[NodeId],
                    title : String,
                    sTooltipText : Option[String] = None)
  extends  BoxPanel(Orientation.Vertical) {
  contents += new Label(title) {
    sTooltipText foreach (this.tooltip = _)
  }

  contents += new ScrollPane {
    val model = DGTreeModel.subGraph(graph, focus)
    val tree: JTree = new JTree(model) with DGTree {
      def icons : DGTreeIcons = treeIcons
    }
    contents = Component.wrap(tree)
  }
}

class NodeInfosPanel
( publisher: Publisher,
  val graph : DependencyGraph,
  val nodeId : NodeId,
  edgeMenuBuilder : NodeIdP => JPopupMenu,
  treeIcons : DGTreeIcons)
  extends BoxPanel(Orientation.Vertical) {

  implicit val g = graph

  background = Color.white

  def addUsesSet(sb : StringBuilder, title : String, usesSet : Set[Uses]) : Unit =
    if(usesSet.nonEmpty) {
      sb.append(title)
      sb.append("\n")
      usesSet.foreach { e =>
        sb append (graph, e).shows
        sb append "\n"
      }
  }

  val node = graph.getConcreteNode(nodeId)

  def mkStringWithNames(nodes : Iterable[NodeId]): String ={
    nodes.toSeq.map{ nid =>
      graph.fullName(nid) + (graph.styp(nid) map (t => " " + (graph, t).shows) getOrElse "")
    }.sorted.mkString("\n", "\n", "\n")
  }

  val typeWeight =
    node.kind.kindType match {
      case TypeDecl =>
        val m = Metrics.typeWeight(graph, nodeId)
        s"Type Weight = ${m.toString}\n"

      case _ => ""
    }





    contents += new Label(s"${node.kind} ${node.name} : " +
      (graph, graph.structuredType(node.id)).shows + s"(${node.id})")
    val providers = Metrics.providers(graph, node.id)
    val clients = Metrics.clients(graph, node.id)
    val internals = Metrics.internalDependencies(graph, node.id).size
    val outgoings = Metrics.outgoingDependencies(graph, node.id).size
    val incomings = Metrics.incomingDependencies(graph, node.id).size
    val coupling = Metrics.coupling0(providers.size, clients.size, internals, outgoings, incomings)
    val cohesion = Metrics.cohesion0(internals, outgoings, incomings)

    def packages(ids : Set[NodeId]) =
      ids.foldLeft(Set[NodeId]())((s, id) =>
        graph.containerOfKindType(NameSpace, id) map ( s + _) getOrElse s)

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

    contents += new TextArea( "Abstractions :" +
      (if(graph.abstractions(node.id).isEmpty) "none\n"
      else mkStringWithNames(graph.abstractions(node.id).flatMap(_.nodes)))){
      tooltip = "Number of direct super types"
      editable = false
    }.leftGlued

    contents += new BoxPanel(Orientation.Horizontal) {
        contents += new GraphTreePane(treeIcons, graph, providers,
          s"Providers : ${providers.size} in ${packages(providers).size} package(s)",
          Some("Nodes of the same kinds which contain a node used by a node contained by this one"))
      contents += new GraphTreePane(treeIcons, graph, clients,
          s"Clients : ${clients.size} in ${packages(clients).size} package(s)",
        Some("Nodes of the same kinds which contain a node using a node contained by this one"))
    }
/*
    contents += new TextArea(  s"Providers : ( ${providers.size} in ${packages(providers).size} package(s))" +
      (if (providers.isEmpty) "none\n"
      else mkStringWithNames(providers))){
      tooltip = "Nodes of the same kinds which contain a node used by a node contained by this one"
      editable = false
    }.leftGlued

    contents += new TextArea(  s"Clients : ( ${clients.size} in ${packages(clients).size} packages)" +
      (if (clients.isEmpty) "none\n"
      else mkStringWithNames(clients)) ){
        tooltip = "Nodes of the same kinds which contain a node using a node contained by this one"
      editable = false
      }.leftGlued
*/

    contents += new BoxPanel(Orientation.Horizontal){
      contents += new Label(f"Coupling = $coupling%.2f"){
        tooltip = "1 - |Clients U Providers| / |Internal + Outgoing + Incoming dependencies |"
      }
      contents += Swing.HGlue
      contents += new Label(f"Cohesion :  $cohesion%.2f"){
        tooltip = "|Internal dependencies| / |Internal + Outgoing + Incoming dependencies|"
      }
    }


    class UsesLabelBox(userId : NodeId, usedId : NodeId,
                       val fullName : String)
    extends Label {

      val sideUses = graph.typeMemberUsesOf(userId, usedId)
      val primaryUses = graph.typeUsesOf(userId, usedId)

      def tag = (sideUses.isEmpty, primaryUses.isEmpty) match {
        case (true, true) => ""
        case (false, true) => "(dominant use)"
        case (true, false) => "(dominated use)"
        case _ => "(both dominant and dominated)"
      }

      text = fullName + " " + tag

          minimumSize = new Dimension(this.size.width, 30)

          listenTo(mouse.clicks)
          reactions += {
            case mc @ MouseClicked(_, point, _, _, _) =>
              val evt = mc.peer
              if(isRightClick(evt)) {
                val menu: JPopupMenu = edgeMenuBuilder((userId, usedId))
                Swing.onEDT(menu.show(this.peer,
                  point.getX.toInt,
                  point.getY.toInt))
              }
              else {
                val sb = new StringBuilder
                sb append (graph, Uses(userId, usedId)).shows
                sb append "\n"
                addUsesSet(sb, "Dominant Uses :", primaryUses)
                addUsesSet(sb, "Dominated Uses :", sideUses)

                publisher publish Log(sb.toString())
              }
          }
      }


    contents +=  new Label("used by:").leftGlued

    contents += new BoxPanel(Orientation.Vertical) {

      val users : Seq[UsesLabelBox] = graph.usersOf(node.id).toSeq map {
        userId =>
          val userDeclId = graph.declarationOf(userId)
          new UsesLabelBox(userId, node.id, graph.fullName(userDeclId))
      }
      users.sortBy(_.fullName).foreach(contents += _)
    }.leftGlued


    val nodeAndAssociates : List[NodeId] = graph nodePlusDefAndParams node.id


    contents +=  new Label("uses :").leftGlued

    nodeAndAssociates foreach {
      id =>
        contents += new BoxPanel(Orientation.Vertical) {
          val used  = graph.usedBy(id).toSeq map (used =>
            new UsesLabelBox(id, used, graph.fullName(used)))

          used.sortBy(_.fullName).foreach(contents += _)
        }.leftGlued
    }

}
