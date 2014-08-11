package puck.gui

import java.io.{PipedInputStream, PipedOutputStream}
import puck.graph.{FilesHandler, AGEdge, AGNode, NodeKind}
import puck.javaAG.JavaNode

import scala.concurrent.Future
import scala.swing._
import scala.swing.event.MouseClicked
import scala.concurrent.ExecutionContext.Implicits.global


/**
 * Created by lorilan on 10/07/14.
 */
class NodeInfosPanel[K <: NodeKind[K]]( val filesHandler : FilesHandler[K],
                      val node : AGNode[K])
  extends SplitPane(Orientation.Horizontal) {

  val useDetails = new BoxPanel(Orientation.Vertical)

  resizeWeight = 0.75

  leftComponent = new BoxPanel(Orientation.Vertical) {
    contents += new Label(node.kind + " : " + node.nameTypeString)

    contents += new Label("used by:")
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

        contents += new Label(user + " " + tag ){
          listenTo(mouse.clicks)
          reactions += {
            case MouseClicked(_,_,_,_,_) =>
              useDetails.contents.clear()
              useDetails.contents += new Label(AGEdge.uses(user, node).toString)

              primaryUsesOpt match {
                case None => ()
                case Some(primaryUses) =>
                  useDetails.contents += new Label("Dominant Uses :")
                  primaryUses.foreach( e => useDetails.contents += new Label(e.toString()))
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

        contents += Button("<o>"){
          Future {
            print("Printing graph ...")

            val pipedOutput = new PipedOutputStream()
            val pipedInput = new PipedInputStream(pipedOutput)

            Future {
              val imgframe = ImageFrame(pipedInput)
              imgframe.visible = true
            }

            //TODO CLEAN CAST !!
            if(filesHandler.makePng(soutput = Some(pipedOutput), selectedUse = Some(AGEdge.uses(user, node))) ==0)
              println("success")
            else
              println("fail")


          }

        }
      }
    }
  }

  rightComponent = useDetails
}
