package puck.gui

import java.awt.Color

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

    contents +=  PuckMainPanel.leftGlued(new Label("used by:"))


    contents += new BoxPanel(Orientation.Vertical) {

      node.users.foreach { user =>

        val sideUsesOpt = user.sideUses.get(node)
        val primaryUsesOpt = user.primaryUses.get(node)

        def tag = (sideUsesOpt, primaryUsesOpt) match {
          case (None, None) => ""
          case (Some(_), None) => "(dominant use)"
          case (None, Some(_)) => "(dominated use)"
          case _ => "(both dominant and dominated)"
        }


        contents += new BoxPanel(Orientation.Horizontal){

          contents += Button("<o>") {
            println("send request !")
            NodeInfosPanel.this.publish(GraphDisplayRequest(Some(AGEdge.uses(user, node))))
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
