package puck.gui.imageDisplay;

import org.apache.batik.dom.events.NodeEventTarget;
import org.apache.batik.dom.svg.*;
import org.apache.batik.svggen.SVGGraphics2DIOException;
import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.swing.gvt.GVTTreeRendererAdapter;
import org.apache.batik.swing.gvt.GVTTreeRendererEvent;
import org.apache.batik.swing.svg.SVGLoadEventDispatcherEvent;
import org.apache.batik.util.SVGConstants;
import org.apache.batik.util.XMLConstants;
import org.apache.batik.util.XMLResourceDescriptor;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.MouseEvent;
import org.w3c.dom.svg.*;
import puck.graph.ConcreteNode;
import puck.graph.DependencyGraph;
import org.apache.batik.svggen.SVGGraphics2D;

import javax.swing.*;
import java.io.*;
import java.awt.BorderLayout;
import java.util.List;

/**
 * Created by lorilan on 3/13/15.
 */



public class SVGPanel extends JPanel{


    /*private void printDoc(){
        System.out.println(canvas.getSVGDocument().toString());
        SVGGraphics2D svgGenerator = new SVGGraphics2D(canvas.getSVGDocument());

        // Finally, stream out SVG to the standard output using UTF-8
        // character to byte encoding
        boolean useCSS = true; // we want to use CSS style attribute
        Writer out = null;
        try {
            out = new OutputStreamWriter(System.out, "UTF-8");
            svgGenerator.stream(out, useCSS);
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        } catch (SVGGraphics2DIOException e) {
            e.printStackTrace();
        }
    }*/

    private SVGController controller;

    private DependencyGraph getGraph(){
        return controller.getGraph();
    }


    class NodeRightClickMenu extends JPopupMenu {

        //Visual actions
        JMenuItem collapse;
        JMenuItem hide;
        //Transfo actions
        JMenuItem move;

        public NodeRightClickMenu(int nodeId){
            ConcreteNode node = getGraph().getConcreteNode(nodeId);
            List<SVGController.AbstractionChoice> choices = controller.abstractionChoices(node);
            //List<JMenuItem> menuChoices = new ArrayList<>();

            JMenuItem it = new JMenuItem("Abstract " + node.name() + " as");
            it.setEnabled(false);
            this.add(it);

            for(SVGController.AbstractionChoice c : choices){
                it = new JMenuItem(c.kind() + "(" + c.policy()+ ")");
                //menuChoices.add(it);
                this.add(it);
            }

            this.addSeparator();
            hide = new JMenuItem("Hide");
            this.add(hide);
            if(getGraph().content(nodeId).nonEmpty()){
                collapse = new JMenuItem("Collapse");
                this.add(collapse);
            }

        }

    }

    class SVGPanelListener implements EventListener {

        private Integer checkIfNodeAndGetId(SVGOMTextElement txtElt){
            if(txtElt.getParentNode().getNodeName().equals("a")){
                SVGOMAElement a = (SVGOMAElement) txtElt.getParentNode();
                try{
                    return Integer.valueOf(a.getHref().getBaseVal());
                }catch(NumberFormatException e ){
                    return null;
                }
            }
            return null;
        }

        private SVGGElement getGroupingNodeParent(Node n0){

            Node n = n0.getParentNode();
            SVGGElement g;
            while(n!=null && !n.getNodeName().equals("g")){
                n = n.getParentNode();
            }
            if(n == null)
                return null;
            else {
                g = (SVGGElement) n;
                if(g.getId().startsWith("node"))
                    return g;
                else
                    return getGroupingNodeParent(g);
            }

        }

        private short LEFTBUTTON = 0;
        private short RIGHTBUTTON = 2;

       /* private Element createRectangle(float x, float y, float width, float height){

            Element fill = canvas.getSVGDocument()
                    .createElementNS(SVGDOMImplementation.SVG_NAMESPACE_URI, "rect");

            fill.setAttributeNS(null, "x", x+"");
            fill.setAttributeNS(null, "y", y+"");
            fill.setAttributeNS(null, "width", width+"");
            fill.setAttributeNS(null, "height", height+"");

            return fill;
        }

        private Element createBackgroundRectangle(SVGOMTextElement txtElt){
            SVGRect rect = txtElt.getBBox();
            Element fill = createRectangle(rect.getX(), rect.getY(), rect.getWidth(), rect.getHeight());

            fill.setAttributeNS(null, "fill", "green");
            fill.setAttributeNS(null, "stroke", "none");

            return fill;
        }*/

        private void handleLeftClick(MouseEvent evt){
            System.out.println("handling left");
            if(evt.getTarget() instanceof SVGTextElement){

                final SVGTextElement txtElt = (SVGTextElement) evt.getTarget();
                System.out.println(canvas.getUpdateManager().getClass());
                System.out.println(canvas.getUpdateManager().getUpdateRunnableQueue().getClass());

                canvas.modify(new Runnable() {
                    public void run() {
                        txtElt.setAttribute("fill", "blue"); //font color

                    }
                });
            }

            /*if(evt.getTarget() instanceof SVGPathElement){
                final SVGPathElement line = (SVGPathElement) evt.getTarget();
                System.out.println(line.getParentNode().getNodeName());
                canvas.modify(new Runnable() {
                    public void run() {
                        line.setAttribute("stroke", "blue"); //font color

                    }
                });
            }*/
        }

        private void handleRightClick(MouseEvent evt){
            if(evt.getTarget() instanceof SVGOMTextElement){
                Integer nodeId = checkIfNodeAndGetId((SVGOMTextElement) evt.getTarget());
                if(nodeId != null){
                    NodeRightClickMenu menu = new NodeRightClickMenu(nodeId);
                    menu.show(SVGPanel.this, evt.getClientX(), evt.getClientY());
                }
            }

            if(evt.getTarget() instanceof SVGOMPathElement){
                SVGOMPathElement line = (SVGOMPathElement) evt.getTarget();
                System.out.println(line.getParentNode().getNodeName());
            }
        }

        @Override
        public void handleEvent(Event evt) {
            MouseEvent mevt = (MouseEvent) evt;
            System.out.println("plop");
            if(mevt.getButton() == RIGHTBUTTON){

                System.out.println("!!!!!!!!!!!!!!!!!");
                Element elt = canvas.getElementById("clust1");

                final NodeList children = elt.getChildNodes();
                canvas.modify(new Runnable() {
                    @Override
                    public void run() {
                        for(int i=0; i< children.getLength(); i++){
                            if(children.item(i) instanceof Element) {
                                Element child = (Element) children.item(i);
                                System.out.println(child.getTagName());
                                if (child.getTagName().equals("polygon")) {
                                    child.setAttributeNS(null, "fill", "red");

                                }
                            }
                        }
                    }
                });


                handleRightClick(mevt);
            }
            if(mevt.getButton() == LEFTBUTTON){
                handleLeftClick(mevt);
            }

        }
    }


    class PUCKSVGCanvas extends JSVGCanvas {
        PUCKSVGCanvas(){
            super();
            setDocumentState(JSVGCanvas.ALWAYS_DYNAMIC);//before loading the document
            userAgent = new BridgeUserAgent(){
                @Override
                public void openLink(SVGAElement elt) {
                    //do nothing
                }
            };

            addGVTTreeRendererListener(new GVTTreeRendererAdapter() {
                @Override
                public void gvtRenderingCompleted(GVTTreeRendererEvent e) {
                    System.out.println("ready !");
                    getRoot().addEventListenerNS(XMLConstants.XML_EVENTS_NAMESPACE_URI,
                            SVGConstants.SVG_EVENT_CLICK,
                            new SVGPanelListener(),
                            false, null);
                }
            });
        }

        Element getElementById(String id){
            return getSVGDocument().getElementById(id);
        }

        void modify(Runnable r){
            getUpdateManager().getUpdateRunnableQueue().invokeLater(r);
        }
    }

    private PUCKSVGCanvas canvas;

    private NodeEventTarget getRoot(){
        return (NodeEventTarget)canvas.getSVGDocument().getRootElement();
    }

    public SVGPanel(SVGDocument doc, SVGController controller){
        canvas = new PUCKSVGCanvas();
        canvas.setSVGDocument(doc);

        this.controller = controller;
        this.setLayout(new BorderLayout());
        this.add("Center", canvas);


    }

    public static SVGPanel fromStream(InputStream stream, SVGController g) throws IOException {
        String parser = XMLResourceDescriptor.getXMLParserClassName();
        SAXSVGDocumentFactory factory = new SAXSVGDocumentFactory(parser);
        SVGDocument doc = factory.createSVGDocument("", stream);
        return new SVGPanel(doc, g);
    }

   /* public static SVGPanel fromFilePath(String filePath ) throws IOException {
        return fromStream(new FileReader(filePath));
    }*/
}
