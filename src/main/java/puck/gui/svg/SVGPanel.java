package puck.gui.svg;

import org.apache.batik.dom.GenericText;
import org.apache.batik.dom.events.NodeEventTarget;
import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.swing.gvt.GVTTreeRendererAdapter;
import org.apache.batik.swing.gvt.GVTTreeRendererEvent;
import org.apache.batik.util.SVGConstants;
import org.apache.batik.util.XMLConstants;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.MouseEvent;
import org.w3c.dom.svg.*;
import puck.graph.DGEdge;
import puck.graph.DependencyGraph;

import javax.swing.*;
import java.awt.BorderLayout;
import java.awt.event.KeyEvent;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SVGPanel extends JPanel{


    private SVGController controller;

    private DependencyGraph getGraph(){
        return controller.graph();
    }


    class SVGPanelListener implements EventListener {

        private Integer checkIfNodeAndGetId(Element txtElt){
            if(txtElt.getParentNode().getNodeName().equals("a")){
                SVGAElement a = (SVGAElement) txtElt.getParentNode();
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

        private SVGGElement checkIfEdgeAndGetGElement(Element path){
            if(path.getParentNode().getNodeName().equals("g")){
                SVGGElement gelt = (SVGGElement) path.getParentNode();
                if(gelt.getId().startsWith("edge")){
                    return gelt;
                }
                return null;
            }
            return null;
        }

        private DGEdge.EKind edgeKindFromGElement(SVGGElement gelt){
            NodeList l = gelt.getChildNodes();
            for(int i = 0; i < l.getLength(); i++){
                Node n = l.item(i);
                if(n.getNodeName().equals("path")){
                    if(((Element)n).hasAttribute("stroke-dasharray"))
                        return controller.isaKind();
                    else
                        return controller.usesKind();
                }
            }
            return controller.usesKind();
        }

        private Pattern arrowPattern =
                Pattern.compile("\\d+:(\\d+).{2}\\d+:(\\d+)");

        private DGEdge edgeFromGElement(SVGGElement gelt){
            SVGTitleElement t = (SVGTitleElement)gelt.getFirstChild();
            String edgeLabel = t.getNodeValue();
            GenericText title = (GenericText)t.getFirstChild();

            controller.console().appendText(edgeLabel);
            DGEdge.EKind k = edgeKindFromGElement(gelt);

            Matcher m = arrowPattern.matcher(title.getData());

            if(m.find()){
                int source = Integer.valueOf(m.group(1));
                int target = Integer.valueOf(m.group(2));
                return k.apply(source, target);
            }

            return null;
        }

        private void changeEdgeColor(SVGGElement edge, String color){
            NodeList l = edge.getChildNodes();
            for(int i = 0; i < l.getLength(); i++){
                Node n = l.item(i);
                if(n.getNodeName().equals("path")){
                    ((Element)n).setAttribute("stroke", color);
                }
                if(n.getNodeName().equals("polygon")){
                    ((Element)n).setAttribute("stroke", color);
                    ((Element)n).setAttribute("fill", color);
                }
            }
        }

        private DGEdge conditionalEdgeReset(){
            if(controller.edgeIsSelected()){
                changeEdgeColor(controller.getEdgeDomElement(),
                        controller.getEdgeColor());
                DGEdge e = controller.getEdgeSelected();
                controller.resetEdgeSelected();
                return e;
            }
            return null;
        }

        private Integer conditionalNodeReset(){
            if(controller.nodeIsSelected()){
                controller.getNodeDomElement()
                        .setAttribute("fill", controller.getNodeColor());
                int i = controller.getIdNodeSelected();
                controller.resetNodeSelected();
                return i;
            }
            return null;
        }

        private String selectColor = "blue";

        private void handleLeftClick(MouseEvent evt){
            System.out.println("handling left");
            if(evt.getTarget() instanceof SVGTextElement){

                final Element txtElt = (SVGTextElement) evt.getTarget();
                final Integer nodeId = checkIfNodeAndGetId(txtElt);
                if(nodeId != null)
                    canvas.modify(new Runnable() {
                        public void run() {
                            conditionalEdgeReset();
                            Integer nid = conditionalNodeReset();
                            if(!nodeId.equals(nid)) {
                                controller.setNodeSelected(nodeId, txtElt);
                                txtElt.setAttribute("fill", selectColor);
                            }

                        }
                    });
            }

            else if((evt.getTarget() instanceof SVGPathElement)
                    || (evt.getTarget() instanceof SVGPolygonElement)){
                final Element line = (Element) evt.getTarget();
                final SVGGElement gedge = checkIfEdgeAndGetGElement(line);
                if(gedge != null) {
                    canvas.modify(new Runnable() {
                        public void run() {
                            conditionalNodeReset();
                            DGEdge prevEdge = conditionalEdgeReset();
                            DGEdge e = edgeFromGElement(gedge);
                            if(e != null && !e.equals(prevEdge)){
                                String color = line.getAttribute("stroke");
                                controller.setEdgeSelected(e, gedge, color);
                                changeEdgeColor(gedge, selectColor);
                            }
                        }
                    });
                }
            }

            else {
                canvas.modify(new Runnable() {
                    public void run() {
                        conditionalNodeReset();
                        conditionalEdgeReset();
                    }
                });
            }
        }

        private void handleRightClick(MouseEvent evt) {
            if (evt.getTarget() instanceof SVGTextElement) {
                Integer nodeId = checkIfNodeAndGetId((Element) evt.getTarget());
                if (nodeId != null) {
                    NodeRightClickMenu menu = new NodeRightClickMenu(controller, nodeId);
                    menu.show(SVGPanel.this, evt.getClientX(), evt.getClientY());
                }
            }

            if ((evt.getTarget() instanceof SVGPathElement)
                    || (evt.getTarget() instanceof SVGPolygonElement)) {
                final Element line = (Element) evt.getTarget();
                final SVGGElement gedge = checkIfEdgeAndGetGElement(line);
                if (gedge != null) {
                    DGEdge e = edgeFromGElement(gedge);
                    EdgeRightClickMenu menu = new EdgeRightClickMenu(controller, e);
                    menu.show(SVGPanel.this, evt.getClientX(), evt.getClientY());
                }
            }
        }
        @Override
        public void handleEvent(Event evt) {
            MouseEvent mevt = (MouseEvent) evt;
            if(mevt.getButton() == RIGHTBUTTON){

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

            KeyStroke key = KeyStroke.getKeyStroke(KeyEvent.VK_EQUALS,
                    KeyEvent.CTRL_DOWN_MASK | KeyEvent.SHIFT_DOWN_MASK);
            getInputMap().put(key, ZOOM_IN_ACTION);
            key = KeyStroke.getKeyStroke(KeyEvent.VK_ADD, KeyEvent.CTRL_DOWN_MASK);
            getInputMap().put(key, ZOOM_IN_ACTION);

            key = KeyStroke.getKeyStroke(KeyEvent.VK_MINUS, KeyEvent.CTRL_MASK);
            getInputMap().put(key, ZOOM_OUT_ACTION);
            key = KeyStroke.getKeyStroke(KeyEvent.VK_SUBTRACT, KeyEvent.CTRL_MASK);
            getInputMap().put(key, ZOOM_OUT_ACTION);

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

    PUCKSVGCanvas canvas;


    private NodeEventTarget getRoot(){
        return (NodeEventTarget)canvas.getSVGDocument().getRootElement();
    }

    public void setController(SVGController controller){
        this.controller = controller;
    }

    public SVGPanel(SVGDocument doc){
        canvas = new PUCKSVGCanvas();
        canvas.setSVGDocument(doc);

        this.setLayout(new BorderLayout());
        this.add("Center", canvas);


    }

}
