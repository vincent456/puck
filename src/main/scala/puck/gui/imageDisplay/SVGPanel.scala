package puck.gui.imageDisplay

import java.awt.Dimension

import org.apache.batik.swing.JSVGCanvas

import scala.swing.{Frame, Component, FlowPanel}

/**
 * Created by lorilan on 3/12/15.
 */
class SVGPanel(filePath : String) extends FlowPanel {
  val canvas = new JSVGCanvas()
  canvas.setURI(filePath)
  contents += Component.wrap(canvas)
}

class SVGFrame(filePath : String) extends Frame {

  size = new Dimension(640, 480)
  contents = new SVGPanel(filePath)
  this.visible = true

}

/*object SVGPanel{

  def apply(filePath : String) : SVGPanel = {
    val parser : String = XMLResourceDescriptor.getXMLParserClassName
    val factory = new SAXSVGDocumentFactory(parser)
    factory.createDocument(filePath, new FileReader(filePath))

    new SVGPanel()
  }
}*/
