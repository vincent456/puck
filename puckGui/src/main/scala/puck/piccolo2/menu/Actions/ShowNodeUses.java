package puck.piccolo2.menu.Actions;

import org.piccolo2d.PNode;
import org.piccolo2d.event.PInputEvent;
import org.piccolo2d.nodes.PText;
import puck.graph.DependencyGraph;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.uses.ArrowNodesHolder;
import puck.piccolo2.uses.Parrow;
import puck.piccolo2.uses.Triangle;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import java.awt.*;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class ShowNodeUses extends MenuItemEventHandler {

    private DependencyGraph DG;
    private ArrowNodesHolder arrowNodesHolder;
    private HashMap<Object,PiccoloCustomNode> idNodeMap;
    private PiccoloCustomNode target;

    public ShowNodeUses( DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object,PiccoloCustomNode> idNodeMap){
        this.DG=DG;
        this.arrowNodesHolder=arrowNodesHolder;
        this.idNodeMap=idNodeMap;
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        int nodeId=target.getidNode();

        Set<Object> usedby=DG.usedBy(nodeId);
        for(Iterator<Object> iterator = usedby.toIterator(); iterator.hasNext();) {
            Object O=iterator.next();
            arrowNodesHolder.addArrow(new Parrow(target, idNodeMap.get(O), new Triangle(Color.BLACK), 5, Color.BLACK));
            System.out.println("used by : "+O.toString()+" "+idNodeMap.get(O).getidNode());
        }
    }

    public void setTarget(PiccoloCustomNode target){
        this.target=target;
    }
}
