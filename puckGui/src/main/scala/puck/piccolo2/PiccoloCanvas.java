package puck.piccolo2;

import org.piccolo2d.PCanvas;
import org.piccolo2d.extras.swing.PScrollPane;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.piccolo2.LayoutStack.LayoutStack;
import puck.piccolo2.menu.DisplayUsesMenu;
import puck.piccolo2.node.NodeAdapterTree;
import puck.piccolo2.node.PCustomInputEventHandler;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.view.NodeKindIcons;

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

        //region piccoloCustomNode
        NodeAdapterTree nta=new NodeAdapterTree(control,0,icons);

        node = new PiccoloCustomNode(nta);
        nta=null;

        canvas.getLayer().addChild(node);

        node.setLayout();
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

        System.out.println("popEvent");
    }

    public void pushEvent(DependencyGraph newGraph,DependencyGraph oldGraph){

        System.out.println("pushedEvent");
    }

    public void evt(){
        System.out.println("evt");
    }

    //endregion
}
