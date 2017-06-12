package puck.piccolo2.menu.Actions.Actions2;

import org.piccolo2d.PNode;
import org.piccolo2d.event.PInputEvent;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.graph.NodeKind;
import puck.graph.transformations.TransformationRules;
import puck.piccolo2.menu.Actions.MenuItemEventHandler;
import scala.Tuple2;

import javax.swing.*;

/**
 * Created by Vincent Hudry on 12/06/2017.
 */
public class AddChildKind extends MenuItemEventHandler{

    private PuckControl control;
    private NodeKind childKind;

    public  AddChildKind(PuckControl control){
        this.control=control;
    }


    @Override
    public void setTarget(PNode target) {
    }

    @Override
    public void mouseClicked(PInputEvent e) {
        String answer= JOptionPane.showInputDialog("New "+childKind.toString()+" name:");
        if(answer==null||answer.equals(""))
            return;
        DependencyGraph DG=control.graph();
        TransformationRules TR=control.graphUtils().Rules();

        Tuple2 ng= TR.intro().apply(DG.mileStone(),answer,childKind);
//        DependencyGraph DG2=DG.addContains();
//        control.historyHandler().pushGraph(DG2);
    }
}
