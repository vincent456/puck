package puck.piccolo2.node;

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

    public PCustomInputEventHandler(PiccoloCustomNode node,PiccoloCustomNode tree, DisplayUsesMenu menu){
        setEventFilter(new PInputEventFilter(InputEvent.BUTTON1_MASK & InputEvent.BUTTON2_MASK));
        this.node=node;
        this.tree=tree;

        PCustomInputEventHandler.menu=menu;
    }

    @Override
    public void mouseClicked(PInputEvent e){
        if(e.isLeftMouseButton()) {
            node.toggleChildren();
            tree.setGridLayoutV();
            //optional
            menu.clear();
        }
        if(e.isRightMouseButton()){
            if(e.getPickedNode() instanceof PText)
            menu.setTarget(e.getPickedNode().getParent().getParent());
            menu.draw(new Point2D.Double(e.getPosition().getX(),e.getPosition().getY()));
        }
    }
}
