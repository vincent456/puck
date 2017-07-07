package puck.piccolo2;

import org.piccolo2d.PCanvas;
import org.piccolo2d.extras.swing.PScrollPane;
import org.piccolo2d.nodes.PPath;
import puck.control.PuckControl;
import puck.graph.Contains;
import puck.graph.DGEdge;
import puck.graph.DependencyGraph;
import puck.piccolo2.LayoutStack.LayoutStack;
import puck.piccolo2.LayoutStack.LayoutState;
import puck.piccolo2.Parrows.Parrow;
import puck.piccolo2.Parrows.ParrowDottedFat;
import puck.piccolo2.Parrows.ParrowFat;
import puck.piccolo2.menu.DisplayUsesMenu;
import puck.piccolo2.node.NodeAdapterTree;
import puck.piccolo2.node.NodeContent;
import puck.piccolo2.node.PCustomInputEventHandler;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.view.NodeKindIcons;

import java.awt.*;
import java.util.Collection;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 29/05/2017.
 */
public class PiccoloCanvas extends PScrollPane {
    private PuckControl control;

    private PiccoloCustomNode node;
    private NodeKindIcons icons;

    private HashMap<Object,PiccoloCustomNode> idNodeMap;

    private ArrowNodesHolder ANH;

    private PCanvas canvas;

    private DisplayUsesMenu menu;

    private PiccoloCustomNode root;

    private LayoutStack layoutStack;

    public PCanvas getCanvas(){
        return canvas;
    }

    public PiccoloCanvas(PuckControl control,NodeKindIcons icons){
        canvas=new PCanvas();
        this.control=control;
        setViewportView(canvas);
        this.icons=icons;

        //region piccoloCustomNode
        NodeAdapterTree nta=new NodeAdapterTree(control.graph(),0,icons);

        node = new PiccoloCustomNode(nta);
        nta=null;

        canvas.getLayer().addChild(node);

        //node.setLayout();
        // node.updateContentBoundingBoxes(true,canvas);

        //endregion

        //region idNodeMap
        idNodeMap=new HashMap<>();
        fillIdNodeMap(node);
        //endregion

        //region ArrowNodesHolder

        ANH=new ArrowNodesHolder();

        canvas.getLayer().addChild(ANH);
        //endregion

        //region menu
        menu=new DisplayUsesMenu(control,ANH,idNodeMap);

        root=node;

        addEvent(node,node,menu);

        canvas.getLayer().addChild(menu);
        //endregion

        //region LayoutStack

        layoutStack=new LayoutStack();

        //endregion

    }

    private void addEvent(PiccoloCustomNode node, PiccoloCustomNode tree,DisplayUsesMenu menu){
        node.getContent().addInputEventListener(new PCustomInputEventHandler(node,tree,menu,canvas,ANH));
        if(node.getAllChildren().size()!=0)
            for(PiccoloCustomNode PCN:node.getAllChildren()){
                addEvent(PCN,tree,menu);
            }
    }

    private void fillIdNodeMap(PiccoloCustomNode node){
        idNodeMap.put(node.getidNode(),node);
        for(PiccoloCustomNode PCN:node.getAllChildren())
            fillIdNodeMap(PCN);
    }

    //region events
    public void popEvent(DependencyGraph newGraph,DependencyGraph oldGraph){

        pushEvent(newGraph,oldGraph);
        //System.out.println("popEvent");
    }

    public void pushEvent(DependencyGraph newGraph,DependencyGraph oldGraph){

        layoutStack.push(new LayoutState(root));
        //region reset

        //TODO: rebind the old arrows instead of deleting them
        ANH=new ArrowNodesHolder();

        NodeAdapterTree NTA=new NodeAdapterTree(newGraph,0,icons);
        root=new PiccoloCustomNode(NTA);
        NTA=null;

        fillIdNodeMap(root);
        menu=new DisplayUsesMenu(control,ANH,idNodeMap);
        addEvent(root,root,menu);


        //endregion

        LayoutState LS=layoutStack.peek();
        LS.setLayout(idNodeMap);

        //region draw
        canvas.getLayer().removeAllChildren();
        canvas.getLayer().addChild(root);
        root.setLayout();
        canvas.getLayer().addChild(menu);
        canvas.getLayer().addChild(ANH);
        //endregion

    }

    public void evt(){
        //System.out.println("evt");
    }

    public void focus(DGEdge edge){

       //System.out.println("focus");
       //System.out.println(edge.source());
       //System.out.println(edge.target());

        PiccoloCustomNode Psrc=idNodeMap.get(edge.source());
        PiccoloCustomNode Pdst=idNodeMap.get(edge.target());


        Psrc.focus();
        Pdst.focus();
        node.showChildren();
        node.setLayout();
        node.updateContentBoundingBoxes(false,canvas);
        for(Parrow parrow:ANH.getVisibleArrows())
        ANH.updatePosition(parrow);
        ANH.clearCounters();
        for(Parrow ar:ANH.getVisibleArrows()){
            if(ar instanceof ParrowDottedFat){
                ANH.updateCount((ParrowDottedFat) ar);
            }
        }

            NodeContent content=idNodeMap.get(edge.target()).getContent();
        if(edge.kind().toString().equals("Contains")&&content.getText().getTextPaint()==Color.RED){
            return;
        }
            if(edge.kind().toString().equals("Contains")&&content.getText().getTextPaint()!=Color.RED)
        {
            content.getText().setTextPaint(Color.RED);
            PPath line=PPath.createLine(0,content.getBounds().getHeight(),content.getBounds().getWidth(),content.getBounds().getHeight());
            line.setPaint(Color.RED);
            content.addChild(line);
        }
        else
        ANH.addArrow(new ParrowFat(Pdst.getContent(),Psrc.getContent(),5, Color.RED));
    }
}
