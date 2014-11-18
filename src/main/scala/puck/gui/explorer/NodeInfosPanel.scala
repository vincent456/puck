package puck.gui.explorer

import puck.graph._
import puck.gui.{GraphDisplayRequest, PuckMainPanel}

import scala.swing._
import scala.swing.event.MouseClicked


/**
 * Created by lorilan on 10/07/14.
 */
class NodeInfosPanel(val graph : AccessGraph,
                     val nodeId : NodeId,
                      printId : () => Boolean,
                      printSig: () => Boolean)
  extends SplitPane(Orientation.Horizontal) {

  val useDetails = new BoxPanel(Orientation.Vertical)

  resizeWeight = 0.75

  val node = graph.getNode(nodeId)

  def mkStringWithNames(nodes : Iterable[NodeId]): String ={
    nodes.map(graph.getNode(_).nameTypeString).mkString("\n", "\n", "\n")
  }

  leftComponent = new BoxPanel(Orientation.Vertical) {
    contents += PuckMainPanel.leftGlued(new Label(node.kind + " : " + node.nameTypeString))
    val prov = node.providers
    val cl = node.clients
    contents += new TextArea("Internal dependencies : " + node.internalDependencies.size + "\n" +
      "Outgoing dependencies : " + node.outgoingDependencies.size + "\n" +
      "Incoming dependencies : " + node.incomingDependencies.size + "\n" +
      "Subtypes : " +
      (if(node.directSubTypes.isEmpty) "none\n"
      else mkStringWithNames(node.directSubTypes)) +
      "SuperTypes :" +
      (if(node.directSuperTypes.isEmpty) "none\n"
      else mkStringWithNames(node.directSuperTypes)) +
      "Providers : " +
      (if (prov.isEmpty) "none\n"
      else mkStringWithNames(prov)) +
      "Clients : " +
      (if (cl.isEmpty) "none\n"
      else mkStringWithNames(cl) +
      f"Coupling = ${node.coupling}%.2f  Cohesion :  ${node.cohesion}%.2f"))

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

      node.users.foreach { user =>

        val sideUses = graph.usesDominatedBy(user, nodeId)
        val primaryUses = graph.usesDominating(user, nodeId)

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
              sUse = Some(AGEdge.uses(user, nodeId)))
          }

          contents += new Label(user + " " + tag) {

            minimumSize = new Dimension(this.size.width, 30)

            listenTo(mouse.clicks)
            reactions += {
              case MouseClicked(_, _, _, _, _) =>

                useDetails.contents.clear()
                useDetails.contents += new Label(AGEdge.uses(user, nodeId).toString)

                if (primaryUses.nonEmpty){
                    useDetails.contents += new Label("Dominant Uses :")
                    primaryUses.foreach(e => useDetails.contents += new Label(e.toString()))
                }

                if(sideUses.nonEmpty) {
                    useDetails.contents += new Label("Dominated Uses :")
                    sideUses.foreach(e => useDetails.contents += new Label(e.toString()))
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
