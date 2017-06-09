package puck.piccolo2.menu.Actions.Actions2;

import org.piccolo2d.event.PInputEvent;
import puck.graph.DependencyGraph;
import puck.graph.transformations.TransformationRules;
import puck.piccolo2.menu.Actions.MenuItemEventHandler;

/**
 * Created by Vincent Hudry on 09/06/2017.
 */
public class Rename extends MenuItemEventHandler{

    DependencyGraph DG;
    TransformationRules TR;

    public Rename(DependencyGraph DG,TransformationRules TR){
        this.DG=DG;
        this.TR=TR;
    }

    @Override
    public void mouseClicked(PInputEvent e) {

    }
}
