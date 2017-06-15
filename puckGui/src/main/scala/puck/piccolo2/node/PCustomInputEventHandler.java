package puck.piccolo2.node;

import org.piccolo2d.PCanvas;
import org.piccolo2d.event.PBasicInputEventHandler;
import org.piccolo2d.event.PInputEvent;
import org.piccolo2d.event.PInputEventFilter;
import org.piccolo2d.nodes.PText;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.piccolo2.Parrows.Parrow;
import puck.piccolo2.menu.DisplayUsesMenu;
import puck.piccolo2.node.PiccoloCustomNode;

import java.awt.event.InputEvent;
import java.awt.geom.Point2D;
import java.util.Collection;

public class PCustomInputEventHandler extends PBasicInputEventHandler {
    private PiccoloCustomNode node;
    private PiccoloCustomNode tree;

    private static DisplayUsesMenu menu;

    private PCanvas canvas;

    private ArrowNodesHolder ANH;

    public PCustomInputEventHandler(PiccoloCustomNode node,PiccoloCustomNode tree, DisplayUsesMenu menu,PCanvas canvas, ArrowNodesHolder ANH){
        setEventFilter(new PInputEventFilter(InputEvent.BUTTON1_MASK & InputEvent.BUTTON2_MASK));
        this.node=node;
        this.tree=tree;

        PCustomInputEventHandler.menu=menu;

        this.canvas=canvas;

        this.ANH=ANH;
    }

    @Override
    public void mouseClicked(PInputEvent e){
        if(e.isLeftMouseButton()) {
            node.toggleChildren();
            tree.setLayout();
            tree.updateContentBoundingBoxes(false,canvas);
            //optional
            menu.clear();;

            //arrows
            //TODO make the arrows (dis)appear when the node is toggled
            //you can use the ArrowNodeHolder ANH

            //region hide arrows
            Collection<PiccoloCustomNode> hierarchy = node.getHierarchy();
            for(PiccoloCustomNode PCN:hierarchy) {
                //System.out.println(PCN.toString()+PCN.isHidden());
                if (PCN.isHidden())
                    for (Parrow arrow : ANH.getVisibleArrows()) {

                        PiccoloCustomNode PCNF=(PiccoloCustomNode) arrow.getFrom().getParent();
                        PiccoloCustomNode PCNT=(PiccoloCustomNode) arrow.getTo().getParent();

                        if(PCN==PCNF||PCN==PCNT)
                            ANH.hideArrow(arrow);
                    }
            }
            //endregion
        }
        if(e.isRightMouseButton()){
            if(e.getPickedNode() instanceof PText)
            menu.draw(e.getPickedNode());
        }
    }
}
