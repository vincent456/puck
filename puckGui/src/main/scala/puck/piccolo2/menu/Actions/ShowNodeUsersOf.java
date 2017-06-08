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
import java.awt.geom.Point2D;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class ShowNodeUsersOf extends MenuItemEventHandler {

    protected DependencyGraph DG;
    protected ArrowNodesHolder arrowNodesHolder;
    protected HashMap<Object,PiccoloCustomNode> idNodeMap;
    protected PiccoloCustomNode target;

    public ShowNodeUsersOf(DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object,PiccoloCustomNode> idNodeMap){
        this.DG=DG;
        this.arrowNodesHolder=arrowNodesHolder;
        this.idNodeMap=idNodeMap;
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        int nodeId=target.getidNode();

        Set<Object> usersof=DG.usersOf(nodeId);
        for(Iterator<Object> iterator = usersof.toIterator(); iterator.hasNext();) {
            Object O=iterator.next();
            PNode from = idNodeMap.get(O).getContent();
            PNode to=target.getContent();
            arrowNodesHolder.addArrow(new Parrow(from,to, new Triangle(Color.YELLOW), 5, Color.YELLOW));
        }
    }

    public void setTarget(PiccoloCustomNode target){
        this.target=target;
    }
}
