package puck.piccolo2;

import org.piccolo2d.PCanvas;
import org.piccolo2d.extras.pswing.PSwingCanvas;
import org.piccolo2d.extras.swing.PScrollPane;
import puck.control.PuckControl;
import puck.graph.GraphUtils;
import puck.piccolo2.menu.DisplayUsesMenu;
import puck.piccolo2.node.NodeAdapterTree;
import puck.piccolo2.node.PCustomInputEventHandler;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.view.NodeKindIcons;

import javax.swing.*;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 29/05/2017.
 */
public class PiccoloCanvas extends PScrollPane {
    private PCanvas canvas;
    private PuckControl control;

    private PiccoloCustomNode node;
    private NodeKindIcons icons;

    private DisplayUsesMenu menu;

    private HashMap<Object,PiccoloCustomNode> idNodeMap;

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

        ArrowNodesHolder arrowNodesHolder=new ArrowNodesHolder();

        canvas.getLayer().addChild(arrowNodesHolder);
        //endregion

        //region menu
        menu=new DisplayUsesMenu(control,arrowNodesHolder,idNodeMap);

        addEvent(node,node,menu);

        canvas.getLayer().addChild(menu);
        //endregion


    }

    private void addEvent(PiccoloCustomNode node, PiccoloCustomNode tree,DisplayUsesMenu menu){
        node.getContent().addInputEventListener(new PCustomInputEventHandler(node,tree,menu,canvas));
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
}
