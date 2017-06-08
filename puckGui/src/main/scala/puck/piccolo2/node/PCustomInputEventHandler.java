package puck.piccolo2.node;

import org.piccolo2d.PCanvas;
import org.piccolo2d.event.PBasicInputEventHandler;
import org.piccolo2d.event.PInputEvent;
import org.piccolo2d.event.PInputEventFilter;
import org.piccolo2d.nodes.PText;
import puck.piccolo2.menu.DisplayUsesMenu;
import puck.piccolo2.node.PiccoloCustomNode;

import java.awt.event.InputEvent;
import java.awt.geom.Point2D;

public class PCustomInputEventHandler extends PBasicInputEventHandler {
    private PiccoloCustomNode node;
    private PiccoloCustomNode tree;

    private static DisplayUsesMenu menu;

    private PCanvas canvas;

    public PCustomInputEventHandler(PiccoloCustomNode node,PiccoloCustomNode tree, DisplayUsesMenu menu,PCanvas canvas){
        setEventFilter(new PInputEventFilter(InputEvent.BUTTON1_MASK & InputEvent.BUTTON2_MASK));
        this.node=node;
        this.tree=tree;

        PCustomInputEventHandler.menu=menu;

        this.canvas=canvas;
    }

    @Override
    public void mouseClicked(PInputEvent e){
        if(e.isLeftMouseButton()) {
            node.toggleChildren();
            tree.setGridLayoutV();
            tree.updateContentBoundingBoxes(false,canvas);
            //optional
            menu.clear();
        }
        if(e.isRightMouseButton()){
            if(e.getPickedNode() instanceof PText)
            menu.draw(e.getPickedNode());
        }
    }
}
