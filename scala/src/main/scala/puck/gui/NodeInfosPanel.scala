package puck.gui

import puck.graph.{AGEdge, AGNode, NodeKind}
import scala.swing._
import scala.swing.event.MouseClicked


/**
 * Created by lorilan on 10/07/14.
 */
class NodeInfosPanel[K <: NodeKind[K]](val node : AGNode[K])
  extends SplitPane(Orientation.Horizontal) {

  val useDetails = new BoxPanel(Orientation.Vertical)

  resizeWeight = 0.75

  leftComponent = new BoxPanel(Orientation.Vertical) {
    contents += PuckMainPanel.leftGlued(new Label(node.kind + " : " + node.nameTypeString))
    val prov = node.providers
    val cl = node.clients
    contents += new TextArea("Internal dependencies : " + node.internalDependencies.size + "\n" +
      "Outgoing dependencies : " + node.outgoingDependencies.size + "\n" +
      "Incoming dependencies : " + node.incomingDependencies.size + "\n" +
      "Subtypes : " +
      (if(node.directSubTypes.isEmpty) "none\n"
      else node.directSubTypes.mkString("\n", "\n", "\n")) +
      "SuperTypes :" +
      (if(node.directSuperTypes.isEmpty) "none\n"
      else node.directSuperTypes.mkString("\n", "\n", "\n")) +
      "Providers : " +
      (if (prov.isEmpty) "none\n"
      else prov.mkString("\n", "\n", "\n")) +
      "Clients : " +
      (if (cl.isEmpty) "none\n"
      else cl.mkString("\n", "\n", "\n")) +
      "Coupling = " + node.coupling + ", Cohesion :  " + node.cohesion)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Move into :")
      val cb = new ComboBox(node.graph.filter(n => (n canContain node) && n != node).toSeq)
      contents += cb
      contents += Button(">>") {
        node.moveTo(cb.selection.item)
        NodeInfosPanel.this.publish(AccessGraphModified(node.graph))
      }
      contents += Swing.HGlue

    }

    contents +=  PuckMainPanel.leftGlued(new Label("used by:"))


    contents += new BoxPanel(Orientation.Vertical) {

      val graph = node.graph
      node.users.foreach { user =>

        val sideUsesOpt = graph.sideUses.get(AGEdge.uses(user, node))
        val primaryUsesOpt = graph.primaryUses.get(AGEdge.uses(user, node))

        def tag = (sideUsesOpt, primaryUsesOpt) match {
          case (None, None) => ""
          case (Some(_), None) => "(dominant use)"
          case (None, Some(_)) => "(dominated use)"
          case _ => "(both dominant and dominated)"
        }


        contents += new BoxPanel(Orientation.Horizontal){

          contents += Button("<o>") {
            println("send request !")
            NodeInfosPanel.this publish
              GraphDisplayRequest("Graph with uses selected",
                sUse = Some(AGEdge.uses(user, node)))
          }

          contents += new Label(user + " " + tag) {

            minimumSize = new Dimension(this.size.width, 30)

            listenTo(mouse.clicks)
            reactions += {
              case MouseClicked(_, _, _, _, _) =>

                useDetails.contents.clear()
                useDetails.contents += new Label(AGEdge.uses(user, node).toString)

                primaryUsesOpt match {
                  case None => ()
                  case Some(primaryUses) =>
                    useDetails.contents += new Label("Dominant Uses :")
                    primaryUses.foreach(e => useDetails.contents += new Label(e.toString()))
                }

                sideUsesOpt match {
                  case None => ()
                  case Some(sideUses) =>
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
