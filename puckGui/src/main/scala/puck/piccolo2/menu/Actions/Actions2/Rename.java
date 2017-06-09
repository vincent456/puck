package puck.piccolo2.menu.Actions.Actions2;

import org.piccolo2d.event.PInputEvent;
import puck.graph.DependencyGraph;
import puck.graph.transformations.TransformationRules;
import puck.piccolo2.menu.Actions.MenuItemEventHandler;

/**
 * Created by Vincent Hudry on 09/06/2017.
 */
public class Rename extends MenuItemEventHandler{

    private DependencyGraph DG;
    private TransformationRules TR;
    private int nodeId;

    public Rename(DependencyGraph DG,TransformationRules TR,int nodeId){
        this.DG=DG;
        this.TR=TR;
        this.nodeId=nodeId;
    }

    @Override
    public void mouseClicked(PInputEvent e) {
        TR.rename();
    }
}
