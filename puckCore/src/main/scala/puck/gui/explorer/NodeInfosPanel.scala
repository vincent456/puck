package puck.gui.explorer

import puck.graph.io.VisibilitySet
import puck.graph._
import puck.gui.{GraphDisplayRequest, PuckMainPanel}
import ShowDG._
import scala.swing._
import scala.swing.event.MouseClicked


class NodeInfosPanel(val graph : DependencyGraph,
                     val nodeId : NodeId,
                      printId : () => Boolean,
                      printSig: () => Boolean,
                      visibility : VisibilitySet.T)
  extends SplitPane(Orientation.Horizontal) {
  implicit val g = graph
  val useDetails = new BoxPanel(Orientation.Vertical)

  resizeWeight = 0.75

  val node = graph.getConcreteNode(nodeId)

  def mkStringWithNames(nodes : Iterable[NodeId]): String ={
    nodes.map{ nid =>
      val node = graph.getNode(nid)
      showDG(graph)(nodeNameTypCord).shows(node)
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

    contents += PuckMainPanel.leftGlued(new Label(node.kind + " : " +
      showDG(graph)(nodeNameTypCord).shows(node)))
    val prov = Metrics.providers(graph, node.id)
    val cl = Metrics.clients(graph, node.id)
    val internals = Metrics.internalDependencies(graph, node.id).size
    val outgoings = Metrics.outgoingDependencies(graph, node.id).size
    val incomings = Metrics.incomingDependencies(graph, node.id).size
    val coupling = Metrics.coupling(graph, node.id)
    val cohesion = Metrics.cohesion(graph, node.id)
    contents += new TextArea(s"Internal dependencies : $internals \n" +
      s"Outgoing dependencies : $outgoings \n" +
      s"Incoming dependencies : $incomings \n" +
      typeWeight +
      "Subtypes : " +
      (if(graph.directSubTypes(node.id).isEmpty) "none\n"
      else mkStringWithNames(graph.directSubTypes(node.id))) +
      "SuperTypes :" +
      (if(graph.directSuperTypes(node.id).isEmpty) "none\n"
      else mkStringWithNames(graph.directSuperTypes(node.id))) +
      "Providers : " +
      (if (prov.isEmpty) "none\n"
      else mkStringWithNames(prov)) +
      "Clients : " +
      (if (cl.isEmpty) "none\n"
      else mkStringWithNames(cl) +
      f"Coupling = $coupling%.2f  Cohesion :  $cohesion%.2f"))

  /*  contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Move into :")
      val cb = new ComboBox((graph.nodes filter {n : AGNode => n canContain nodeId}).toSeq)
      contents += cb
      contents += Button(">>") {
        throw new AGError("node.moveTo(cb.selection.item) not implemented")
        //node.moveTo(cb.selection.item)
        NodeInfosPanel.this.publish(AccessGraphModified(node.graph))
      }
      contents += Swing.HGlue

    }*/

    contents +=  PuckMainPanel.leftGlued(new Label("used by:"))


    contents += new BoxPanel(Orientation.Vertical) {

      graph.usersOf(node.id).foreach { userId =>

        val sideUses = graph.typeMemberUsesOf(userId, nodeId)
        val primaryUses = graph.typeUsesOf(userId, nodeId)

        def tag = (sideUses.isEmpty, primaryUses.isEmpty) match {
          case (true, true) => ""
          case (false, true) => "(dominant use)"
          case (true, false) => "(dominated use)"
          case _ => "(both dominant and dominated)"
        }


        contents += new BoxPanel(Orientation.Horizontal){

          contents += Button("<o>") {
            NodeInfosPanel.this publish
              GraphDisplayRequest("Graph with uses selected",
              graph, printId(), printSig(),
              visibility,
              sUse = Some(Uses(userId, nodeId)))
          }

          contents += new Label(graph.fullName(userId) + " " + tag) {

            minimumSize = new Dimension(this.size.width, 30)

            listenTo(mouse.clicks)
            reactions += {
              case MouseClicked(_, _, _, _, _) =>

                useDetails.contents.clear()
                useDetails.contents +=
                  new Label(showDG[DGEdge](graph).shows(Uses(userId, nodeId)))

                if (primaryUses.nonEmpty){
                    useDetails.contents += new Label("Dominant Uses :")
                    primaryUses.foreach(e =>
                      useDetails.contents += new Label(showDG[DGEdge](graph).shows(e)))
                }

                if(sideUses.nonEmpty) {
                    useDetails.contents += new Label("Dominated Uses :")
                    sideUses.foreach(e =>
                      useDetails.contents += new Label(showDG[DGEdge](graph).shows(e)))
                }

                useDetails.revalidate()
            }
          }

          contents += Swing.HGlue
        }
      }
    }
  }

  rightComponent = useDetails
}
