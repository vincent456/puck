package puck.piccolo2.menu.Actions;

import org.piccolo2d.event.PInputEvent;
import puck.control.PuckControl;
import puck.graph.*;
import puck.graph.transformations.TransformationRules;

import java.util.ArrayList;
import java.util.List;


/**
 * Created by Vincent Hudry on 21/06/2017.
 */
public class Abstraction_idee1 extends MenuItemEventHandler {

    private PuckControl control;
    private DGNode node;
    private AbstractionPolicy policy;
    private NodeKind absKind;
    private DependencyGraph graph;
    private GraphUtils graphUtils;

    private TransformationRules TR;

    public Abstraction_idee1(PuckControl control, DGNode node, AbstractionPolicy policy, NodeKind absKind, DependencyGraph graph, GraphUtils graphUtils) {
        this.control = control;
        this.node = node;
        this.policy = policy;
        this.absKind = absKind;
        this.graph = graph;
        this.graphUtils = graphUtils;

        TR=graphUtils.Rules();
    }


    @Override
    public void mouseClicked(PInputEvent e) {
        List<ConcreteNode> contentToAbstract=new ArrayList<>();

    }
}
