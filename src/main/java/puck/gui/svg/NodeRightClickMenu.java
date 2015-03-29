package puck.gui.svg;

import puck.graph.ConcreteNode;
import puck.graph.DGEdge;
import puck.graph.DependencyGraph;
import puck.graph.transformations.MergeMatcher;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.util.List;

/**
* Created by lorilan on 3/18/15.
*/
class NodeRightClickMenu extends JPopupMenu {

    final private SVGController controller;

    private DependencyGraph getGraph(){
        return controller.getGraph();
    }
    //Visual actions
    JMenuItem collapse;
    //Transfo actions
    JMenuItem move;

    public NodeRightClickMenu(SVGController controller, int nodeId){
        this.controller = controller;
        final ConcreteNode node = getGraph().getConcreteNode(nodeId);
        //List<JMenuItem> menuChoices = new ArrayList<>();

        JMenuItem item = new JMenuItem("Abstract " + node.name() + " as");
        item.setEnabled(false);
        this.add(item);

        for(JMenuItem it : controller.abstractionChoices(node)){
            this.add(it);
        }


        List<JMenuItem> childChoices = controller.childChoices(node);

        if(! childChoices.isEmpty()) {
            this.addSeparator();
            for (JMenuItem it : childChoices) {
                this.add(it);
            }
        }

        this.addSeparator();
        this.add(new JMenuItem(new AbstractAction("Hide"){
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                NodeRightClickMenu.this.controller.hide(node.id());
            }
        }));

        if(getGraph().content(nodeId).nonEmpty()){
            this.add(new JMenuItem(new AbstractAction("Collapse") {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    NodeRightClickMenu.this.controller.collapse(node.id());
                }
            }));

            this.add(new JMenuItem(new AbstractAction("Expand") {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    NodeRightClickMenu.this.controller.expand(node.id());
                }
            }));
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
