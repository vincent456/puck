package puck.piccolo2.menu.Actions;

import org.piccolo2d.PNode;
import org.piccolo2d.event.PInputEvent;
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
 * Created by Vincent Hudry on 08/06/2017.
 */
public class ShowNodeUsedBy extends ShowNodeUsersOf {

    public ShowNodeUsedBy(DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object, PiccoloCustomNode> idNodeMap) {
        super(DG, arrowNodesHolder, idNodeMap);
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        int nodeId=target.getidNode();

        Set<Object> usersof=DG.usedBy(nodeId);
        for(Iterator<Object> iterator = usersof.toIterator(); iterator.hasNext();) {
            Object O=iterator.next();
            PNode to=idNodeMap.get(O).getContent();
            PNode from = target.getContent();
            arrowNodesHolder.addArrow(new Parrow(from,to, new Triangle(Color.YELLOW), 5, Color.YELLOW));
        }
    }
}
