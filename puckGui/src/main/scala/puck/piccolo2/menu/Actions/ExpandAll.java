package puck.piccolo2.menu.Actions;

import org.piccolo2d.event.PInputEvent;
import puck.piccolo2.node.PiccoloCustomNode;

/**
 * Created by Vincent Hudry on 16/06/2017.
 */
public class ExpandAll extends MenuItemEventHandler{

    private PiccoloCustomNode root;

    public ExpandAll(PiccoloCustomNode root){
        this.root=root;
    }

    @Override
    public void mouseClicked(PInputEvent e) {
        ((PiccoloCustomNode)(target.getParent().getParent())).expandAll();
        root.setLayout();
    }
}