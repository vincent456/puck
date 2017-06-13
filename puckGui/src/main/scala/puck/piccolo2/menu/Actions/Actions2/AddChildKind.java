package puck.piccolo2.menu.Actions.Actions2;

import org.piccolo2d.event.PInputEvent;
import puck.control.PuckControl;
import puck.graph.ConcreteNode;
import puck.graph.DependencyGraph;
import puck.graph.NodeKind;
import puck.graph.transformations.TransformationRules;
import puck.piccolo2.menu.Actions.MenuItemEventHandler;
import puck.piccolo2.node.PiccoloCustomNode;
import scala.Tuple2;

import javax.swing.*;

/**
 * Created by Vincent Hudry on 12/06/2017.
 */
public class AddChildKind extends MenuItemEventHandler{

    private PuckControl control;
    private NodeKind childKind;

    public  AddChildKind(PuckControl control,NodeKind childKind){
        this.control=control;
        this.childKind=childKind;
    }

    @Override
    public void mouseClicked(PInputEvent e) {
        PiccoloCustomNode PCN= (PiccoloCustomNode) target.getParent().getParent();
        String answer= JOptionPane.showInputDialog("New "+childKind.toString()+" name:");
        if(answer==null||answer.equals(""))
            return;
        DependencyGraph DG=control.graph();
        TransformationRules TR=control.graphUtils().Rules();

        ConcreteNode host = DG.getConcreteNode(PCN.getidNode());

        Tuple2 ng= TR.intro().apply(DG.mileStone(),answer,childKind);
        ConcreteNode n = (ConcreteNode) ng._1();
        DependencyGraph g = (DependencyGraph) ng._2();
        DependencyGraph DG2=g.addContains(host.id(),n.id(),false);
        control.historyHandler().pushGraph(DG2);
    }
}
