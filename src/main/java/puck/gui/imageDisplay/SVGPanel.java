package puck.gui.imageDisplay;

import org.apache.batik.dom.events.NodeEventTarget;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.util.SVGConstants;
import org.apache.batik.util.XMLConstants;
import org.apache.batik.util.XMLResourceDescriptor;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.svg.SVGDocument;
import org.w3c.dom.svg.SVGSVGElement;

import javax.swing.JPanel;
import java.awt.*;
import java.io.*;

/**
 * Created by lorilan on 3/13/15.
 */
public class SVGPanel extends JPanel{

    class SVGPanelListener implements EventListener {

        @Override
        public void handleEvent(Event evt) {
            System.out.println(evt.getType() + " happen on "+ evt.getTarget());
        }
    }

    private JSVGCanvas canvas;

    private NodeEventTarget getRoot(){
        return (NodeEventTarget)canvas.getSVGDocument().getRootElement();
    }

    public SVGPanel(SVGDocument doc){
        canvas = new JSVGCanvas();

        this.setLayout(new BorderLayout());
        canvas.setSVGDocument(doc);
        this.add("Center", canvas);

        getRoot().addEventListenerNS(XMLConstants.XML_EVENTS_NAMESPACE_URI,
                                    SVGConstants.SVG_EVENT_CLICK,
                                    new SVGPanelListener(),
                                    false, null);
    }

    public static SVGPanel fromStream(InputStream stream) throws IOException {
        String parser = XMLResourceDescriptor.getXMLParserClassName();
        SAXSVGDocumentFactory factory = new SAXSVGDocumentFactory(parser);
        SVGDocument doc = factory.createSVGDocument("", stream);
        return new SVGPanel(doc);
    }

   /* public static SVGPanel fromFilePath(String filePath ) throws IOException {
        return fromStream(new FileReader(filePath));
    }*/
}
