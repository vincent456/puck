package puck.view

import java.awt.Dimension
import java.io.{ByteArrayInputStream, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets

import org.w3c.dom.Element
import org.w3c.dom.events.{Event, EventListener, MouseEvent}
import org.w3c.dom.svg.{SVGAElement, SVGTextElement}
import puck.control.{GraphChangeEvent, PuckControl, SearchSpace}
import puck.graph.io.{DotPrinter, Svg}
import puck.view
import puck.view.svg.actions.DefaultSwingService
import puck.view.svg.PUCKSVGCanvas

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.{Component, FlowPanel, Swing}


/**
  * Created by Loïc Girault on 12/8/16.
  */
class SearchSpacePane
(control : PuckControl,
 sp : SearchSpace) extends FlowPanel {

  this.preferredSize = new Dimension(PuckMainPanel.width * 3/8, PuckMainPanel.height * 1/6)
  val canvas = new PUCKSVGCanvas() {

    def checkIfNodeAndGetId(txtElt: Element): Option[Int] =
      if (txtElt.getParentNode.getNodeName == "a") {
        val a: SVGAElement = txtElt.getParentNode.asInstanceOf[SVGAElement]
        try Some(a.getHref.getBaseVal.toInt)
        catch {
          case e: NumberFormatException => None
        }
      } else None

      val eventListener : EventListener = new EventListener {
        def handleEvent(evt: Event): Unit = {
          val mevt: MouseEvent = evt.asInstanceOf[MouseEvent]
          DefaultSwingService.swingInvokeLater(() =>
            mevt.getButton match {
              case _ => mevt.getTarget match {
                case txtElt: SVGTextElement =>
                  checkIfNodeAndGetId(txtElt) foreach sp.setAsCurrentGraph
                case _ => ()
              }
            })
        }
      }

  }

  contents += Component.wrap(canvas)

  def genDocument() : Unit = {
    val dotString = sp.toDot(control.currentMetric)
    val stream = new ByteArrayInputStream(dotString.getBytes(StandardCharsets.UTF_8))
    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    Swing onEDT  {
      canvas.setDocument(view.svg.documentFromStream(pipedInput))
    }

    Future {
      (DotPrinter.dotProcessBuilderFromInputStream(stream, Svg) #> pipedOutput).!
    }

  }

  this listenTo control.Bus

  reactions += {
    case _ : GraphChangeEvent => genDocument()
  }
  genDocument()
}
