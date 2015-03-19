package puck.gui.svg;

import puck.graph.ConcreteNode;
import puck.graph.DGEdge;
import puck.graph.DependencyGraph;
import puck.graph.transformations.MergeMatcher;

import javax.swing.*;

/**
* Created by lorilan on 3/18/15.
*/
class NodeRightClickMenu extends JPopupMenu {

    private SVGController controller;

    private DependencyGraph getGraph(){
        return controller.getGraph();
    }
    //Visual actions
    JMenuItem collapse;
    JMenuItem hide;
    //Transfo actions
    JMenuItem move;

    public NodeRightClickMenu(SVGController controller, int nodeId){
        this.controller = controller;
        ConcreteNode node = getGraph().getConcreteNode(nodeId);
        //List<JMenuItem> menuChoices = new ArrayList<>();

        JMenuItem item = new JMenuItem("Abstract " + node.name() + " as");
        item.setEnabled(false);
        this.add(item);

        for(JMenuItem it : controller.abstractionChoices(node)){
            this.add(it);
        }

        this.addSeparator();
        hide = new JMenuItem("Hide");
        this.add(hide);
        if(getGraph().content(nodeId).nonEmpty()){
            collapse = new JMenuItem("Collapse");
            this.add(collapse);
        }

        if(controller.nodeIsSelected()){
            int id = controller.getNodeSelected();
            ConcreteNode selected = getGraph().getConcreteNode(id);
            Action action;
            if(getGraph().canContain(node, selected)){
                action = new MoveAction(node, selected, getGraph(), controller);
                this.add(new JMenuItem(action));
            }

            MergeMatcher m = controller.transfoRules().mergeMatcher(selected);
            if(m.canBeMergedInto(node, getGraph())){
                action = new MergeAction(selected, node, getGraph(), controller);
                this.add(new JMenuItem(action));
            }
        }

        if(controller.edgeIsSelected()){
            DGEdge edge = controller.getEdgeSelected();
            Action action = new RedirectAction(node, edge, controller.supertypePolicy(), controller);
            this.add(new JMenuItem(action));
            action = new RedirectAction(node, edge, controller.delegatePolicy(), controller);
            this.add(new JMenuItem(action));
        }

    }

}
