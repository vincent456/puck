package puck.piccolo2.menu.Actions.Actions2;

import org.piccolo2d.event.PInputEvent;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.graph.transformations.TransformationRules;
import puck.piccolo2.menu.Actions.MenuItemEventHandler;
import puck.piccolo2.node.PiccoloCustomNode;

import javax.swing.*;


/**
 * Created by Vincent Hudry on 09/06/2017.
 */
public class Rename extends MenuItemEventHandler{

    private DependencyGraph DG;
    private TransformationRules TR;
    private PuckControl control;

    public Rename(PuckControl control){
        DG=control.graph();
        TR=control.graphUtils().Rules();
        this.control=control;
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        String dialout = JOptionPane.showInputDialog("Rename","");

        int nodeId=((PiccoloCustomNode)(target.getParent().getParent())).getidNode();
        DependencyGraph DG2 = TR.rename().apply(DG,nodeId,dialout);

        System.out.println(DG.getNode(nodeId));
        System.out.println(DG2.getNode(nodeId));
    }
}
