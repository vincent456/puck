package puck.gui.imageDisplay;

import org.apache.batik.dom.events.NodeEventTarget;
import org.apache.batik.dom.svg.*;
import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.util.SVGConstants;
import org.apache.batik.util.XMLConstants;
import org.apache.batik.util.XMLResourceDescriptor;
import org.w3c.dom.Node;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.MouseEvent;
import org.w3c.dom.svg.*;
import puck.graph.ConcreteNode;
import puck.graph.DependencyGraph;

import javax.swing.*;
import javax.swing.event.AncestorListener;
import javax.swing.text.AbstractDocument;
import java.awt.event.ComponentListener;
import java.awt.event.MouseListener;
import java.io.*;
import java.awt.BorderLayout;
import java.util.List;

/**
 * Created by lorilan on 3/13/15.
 */



public class SVGPanel extends JPanel{


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

        private SVGPolygonElement createBackgroundRectangle(SVGOMTextElement txtElt){
            SVGOMPolygonElement fill = new SVGOMPolygonElement(null,
                    (org.apache.batik.dom.AbstractDocument) canvas.getSVGDocument());

            SVGRect rect = txtElt.getBBox();

            fill.setAttribute("fill", "blue");
            fill.setAttribute("stroke", "none");
            StringBuilder points = new StringBuilder();

            points.append(rect.getX());
            points.append(',');
            points.append(rect.getY());
            points.append(' ');

            points.append(rect.getX());
            points.append(',');
            points.append(rect.getY()+rect.getWidth());
            points.append(' ');

            points.append(rect.getX()+rect.getHeight());
            points.append(',');
            points.append(rect.getY()+rect.getWidth());
            points.append(' ');

            points.append(rect.getX()+rect.getHeight());
            points.append(',');
            points.append(rect.getY());

            fill.setAttribute("points", points.toString());
            //fill.setAttribute();
            return fill;
        }

        private void handleLeftClick(MouseEvent evt){
            System.out.println("handling left");
            if(evt.getTarget() instanceof SVGOMTextElement){

                SVGOMTextElement txtElt = (SVGOMTextElement) evt.getTarget();
                Integer nodeId = checkIfNodeAndGetId(txtElt);
                SVGPolygonElement p = createBackgroundRectangle(txtElt);
                SVGGElement g = getGroupingNodeParent(txtElt);
                System.out.println(g.getId());
                System.out.println(p.getAttribute("points"));
                g.insertBefore(p, g.getFirstChild());
            }

            if(evt.getTarget() instanceof SVGOMPathElement){
                SVGOMPathElement line = (SVGOMPathElement) evt.getTarget();
                System.out.println(line.getParentNode().getNodeName());
            }
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
            System.out.println(evt.getType() + " happen on "+ evt.getTarget());
            MouseEvent mevt = (MouseEvent) evt;

            if(mevt.getButton() == RIGHTBUTTON){
                handleRightClick(mevt);
            }
            if(mevt.getButton() == LEFTBUTTON){
                handleLeftClick(mevt);
            }

        }
    }


    class DeadLinkCanvas extends JSVGCanvas {
        DeadLinkCanvas(){
            super();
            userAgent = new BridgeUserAgent(){
                @Override
                public void openLink(SVGAElement elt) {
                    //do nothing
                }
            };
        }

    }
    private JSVGCanvas canvas;

    private NodeEventTarget getRoot(){
        return (NodeEventTarget)canvas.getSVGDocument().getRootElement();
    }

    public SVGPanel(SVGDocument doc, SVGController controller){
        canvas = new DeadLinkCanvas();
        this.controller = controller;

        this.setLayout(new BorderLayout());
        canvas.setSVGDocument(doc);
        this.add("Center", canvas);



        System.out.println("Ancestor listener");
        for(AncestorListener l :canvas.getAncestorListeners()){
            System.out.println(l.getClass());
        }
        System.out.println("Component listener");
        for(ComponentListener l: canvas.getComponentListeners()){
            System.out.println(l.getClass());
        }
        System.out.println("Mouse listener");
        for(MouseListener l: canvas.getMouseListeners()){
        System.out.println(l.getClass());
        }

            getRoot().addEventListenerNS(XMLConstants.XML_EVENTS_NAMESPACE_URI,
                    SVGConstants.SVG_EVENT_CLICK,
                    new SVGPanelListener(),
                    false, null);
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
