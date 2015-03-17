package puck.gui.imageDisplay;



import java.awt.BorderLayout;
import java.awt.FlowLayout;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.*;
import javax.swing.*;

import org.apache.batik.dom.events.NodeEventTarget;
import org.apache.batik.dom.svg.SVGDOMImplementation;
import org.apache.batik.dom.svg.SVGOMDocument;
import org.apache.batik.dom.svg.SVGOMElement;
import org.apache.batik.dom.svg.SVGOMTextElement;
import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.swing.gvt.GVTTreeRendererAdapter;
import org.apache.batik.swing.gvt.GVTTreeRendererEvent;
import org.apache.batik.swing.svg.SVGDocumentLoaderAdapter;
import org.apache.batik.swing.svg.SVGDocumentLoaderEvent;
import org.apache.batik.swing.svg.GVTTreeBuilderAdapter;
import org.apache.batik.swing.svg.GVTTreeBuilderEvent;
import org.apache.batik.util.SVGConstants;
import org.apache.batik.util.XMLConstants;
import org.w3c.dom.Element;
import org.w3c.dom.svg.SVGAElement;
import org.w3c.dom.svg.SVGElement;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.MouseEvent;
import org.w3c.dom.svg.SVGRect;

public class SVGTest {

    public static void main(String[] args) {
        JFrame f = new JFrame("Batik");
        SVGTest app = new SVGTest(f);
        f.getContentPane().add(app.createComponents());

        f.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        f.setSize(400, 400);
        f.setVisible(true);
    }

    JFrame frame;
    JButton button = new JButton("Load...");
    JLabel label = new JLabel();

    class DeadLinkCanvas extends JSVGCanvas {
        DeadLinkCanvas(){
            super();
            setDocumentState(JSVGCanvas.ALWAYS_DYNAMIC);//before loading the document
            userAgent = new BridgeUserAgent(){
                @Override
                public void openLink(SVGAElement elt) {
                    //do nothing
                }
            };
        }

    }

    JSVGCanvas svgCanvas = new DeadLinkCanvas();

    private NodeEventTarget getRoot(){
        return (NodeEventTarget)svgCanvas.getSVGDocument().getRootElement();
    }

    public SVGTest(JFrame f) {
        frame = f;
        svgCanvas.setDocumentState(JSVGCanvas.ALWAYS_DYNAMIC);
    }

    public JComponent createComponents() {
        final JPanel panel = new JPanel(new BorderLayout());

        JPanel p = new JPanel(new FlowLayout(FlowLayout.LEFT));
        p.add(button);
        p.add(label);

        panel.add("North", p);
        panel.add("Center", svgCanvas);

        // Set the button action.
        button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                JFileChooser fc = new JFileChooser(".");
                int choice = fc.showOpenDialog(panel);
                if (choice == JFileChooser.APPROVE_OPTION) {
                    File f = fc.getSelectedFile();
                    try {
                        svgCanvas.setURI(f.toURL().toString());
                    } catch (IOException ex) {
                        ex.printStackTrace();
                    }
                }
            }
        });

        // Set the JSVGCanvas listeners.
        svgCanvas.addSVGDocumentLoaderListener(new SVGDocumentLoaderAdapter() {
            public void documentLoadingStarted(SVGDocumentLoaderEvent e) {
                label.setText("Document Loading...");
            }
            public void documentLoadingCompleted(SVGDocumentLoaderEvent e) {
                label.setText("Document Loaded.");
            }
        });

        svgCanvas.addGVTTreeBuilderListener(new GVTTreeBuilderAdapter() {
            public void gvtBuildStarted(GVTTreeBuilderEvent e) {
                label.setText("Build Started...");
            }
            public void gvtBuildCompleted(GVTTreeBuilderEvent e) {
                label.setText("Build Done.");
                frame.pack();
            }
        });

        svgCanvas.addGVTTreeRendererListener(new GVTTreeRendererAdapter() {
            public void gvtRenderingPrepare(GVTTreeRendererEvent e) {
                label.setText("Rendering Started...");
            }
            public void gvtRenderingCompleted(GVTTreeRendererEvent e) {
                label.setText("");
                getRoot().addEventListenerNS(XMLConstants.XML_EVENTS_NAMESPACE_URI,
                        SVGConstants.SVG_EVENT_CLICK,
                        new SVGPanelListener(),
                        false, null);
            }
        });

        return panel;
    }

    private Element createRectangle(float x, float y, float width, float height){

        Element fill = svgCanvas.getSVGDocument()
                .createElementNS(SVGDOMImplementation.SVG_NAMESPACE_URI, "rect");


        fill.setAttributeNS(null, "fill", "blue");
        fill.setAttributeNS(null, "stroke", "none");

        fill.setAttributeNS(null, "x", x+"");
        fill.setAttributeNS(null, "y", y+"");
        fill.setAttributeNS(null, "width", width+"");
        fill.setAttributeNS(null, "height", height+"");

        return fill;
    }

    class SVGPanelListener implements EventListener {

        private short LEFTBUTTON = 0;
        private short RIGHTBUTTON = 2;

        public void handleEvent(Event evt) {
            MouseEvent mevt = (MouseEvent) evt;

             if(mevt.getButton() == LEFTBUTTON){

                 svgCanvas.getUpdateManager()
                         .getUpdateRunnableQueue().invokeLater
                         (new Runnable() {
                             public void run() {

                                 System.out.println("!!!!!!!!!!!!!!!!!");
                                 SVGOMDocument doc = (SVGOMDocument)svgCanvas.getSVGDocument();
                                 Element elt = doc.getElementById("polyking");
                                 elt.setAttributeNS(null, "fill", "red");


                             }});
            }
            if(mevt.getButton() == RIGHTBUTTON){

                svgCanvas.getUpdateManager()
                        .getUpdateRunnableQueue().invokeLater
                        (new Runnable() {
                            public void run() {

                                System.out.println("!!!!!!!!!!!!!!!!!");
                                SVGOMDocument doc = (SVGOMDocument)svgCanvas.getSVGDocument();
                                Element elt = doc.getElementById("node3");
                                doc.getElementById("node3bg").setAttributeNS(null, "fill", "blue");

                                elt.appendChild(createRectangle((float)539.5, (float)-319.5, 100, 100));

                            }});
            }

        }
    }
}

/*import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.dom.svg.SVGDOMImplementation;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;


import java.awt.*;
import java.net.URL;

import javax.swing.*;

import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.util.XMLResourceDescriptor;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

*//**
 * A applet demonstrating the JSVGCanvas.
 *
 * @version $Id: AppletDemo.java 985243 2010-08-13 15:30:25Z helder $
 *//*
public class SVGTest extends JFrame{

    SVGTest(){
        this.setSize(640, 480);
        this.setVisible(true);
        this.setLayout(new BorderLayout());
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        canvas = new JSVGCanvas();
        canvas.setDocumentState(JSVGCanvas.ALWAYS_DYNAMIC);
        doc = initDoc();
        canvas.setDocument(doc);

        this.add(canvas, BorderLayout.NORTH);

    }

    public static void main(String[] args){

        SVGTest t = new SVGTest();

    }

    protected JSVGCanvas canvas;

    protected Document doc;

    protected Element svg;

    public Document initDoc() {



        String svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI;
        DOMImplementation impl = SVGDOMImplementation.getDOMImplementation();
        Document doc = impl.createDocument(svgNS, "svg", null);

        // Get the root element (the 'svg' element).
        Element svgRoot = doc.getDocumentElement();

        // Set the width and height attributes on the root 'svg' element.
        svgRoot.setAttributeNS(null, "width", "400");
        svgRoot.setAttributeNS(null, "height", "450");

        // Create the rectangle.
        Element rectangle = doc.createElementNS(svgNS, "rect");
        rectangle.setAttributeNS(null, "x", "10");
        rectangle.setAttributeNS(null, "y", "20");
        rectangle.setAttributeNS(null, "width", "100");
        rectangle.setAttributeNS(null, "height", "50");
        rectangle.setAttributeNS(null, "fill", "red");

        // Attach the rectangle to the root 'svg' element.
        svgRoot.appendChild(rectangle);

        return doc;
    }

    public void destroy() {
        canvas.dispose();
    }

    public void updateBar(final String name, final float value) {
        canvas.getUpdateManager().getUpdateRunnableQueue().invokeLater
                (new Runnable() {
                    public void run() {
                        Element bar = doc.getElementById(name);
                        if (bar == null) {
                            return;
                        }

                        Node n;
                        Element path1, path2, path3;
                        for (n = bar.getFirstChild();
                             n.getNodeType() != Node.ELEMENT_NODE;
                             n = n.getNextSibling()) {
                        }
                        path1 = (Element) n;
                        for (n = n.getNextSibling();
                             n.getNodeType() != Node.ELEMENT_NODE;
                             n = n.getNextSibling()) {
                        }
                        path2 = (Element) n;
                        for (n = n.getNextSibling();
                             n.getNodeType() != Node.ELEMENT_NODE;
                             n = n.getNextSibling()) {
                        }
                        path3 = (Element) n;

                        int offset;
                        if (name.equals("ShoeBar")) {
                            offset = 0;
                        } else if (name.equals("CarBar")) {
                            offset = 79;
                        } else if (name.equals("TravelBar")) {
                            offset = 158;
                        } else {
                            offset = 237;
                        }

                        String d =
                                "M " + (offset + 86) + ",240 v -" + (3.7 * value) + " l 15,-15 v " + (3.7 * value) + " l -15,15 z";
                        path1.setAttributeNS(null, "d", d);
                        d = "M " + (offset + 86) + "," + (240 - 3.7 * value) + " h -39 l 15,-15 h 39 l -15,15 z";
                        path2.setAttributeNS(null, "d", d);
                        d = "M " + (offset + 47) + "," + (240 - 3.7 * value) + " v " + (3.7 * value) + " h 39 v -" + (3.7 * value) + " h -39 z";
                        path3.setAttributeNS(null, "d", d);
                    }
                });
    }
}*/
