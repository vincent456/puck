package puck.piccolo2.menu.Actions;

import org.piccolo2d.PNode;
import org.piccolo2d.event.PInputEvent;
import puck.graph.DependencyGraph;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.*;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import java.awt.*;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class ShowNodeUsersOf extends MenuItemEventHandler {

    protected DependencyGraph DG;
    protected ArrowNodesHolder arrowNodesHolder;
    protected HashMap<Object,PiccoloCustomNode> idNodeMap;

    public ShowNodeUsersOf(DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object,PiccoloCustomNode> idNodeMap){
        this.DG=DG;
        this.arrowNodesHolder=arrowNodesHolder;
        this.idNodeMap=idNodeMap;
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        int nodeId=target.getidNode();

        Set<Object> usersof=DG.usersOf(nodeId);
        for(Iterator<Object> iterator = usersof.toIterator(); iterator.hasNext();) {
            Object O=iterator.next();
            PNode from=target.getContent();
            PNode to=idNodeMap.get(O).getContent();
            if((to.getParent().getParent() instanceof  PiccoloCustomNode)&&!((PiccoloCustomNode) to.getParent().getParent()).isHidden())
            arrowNodesHolder.addArrow(new ParrowUses(from,to,10));
        }
    }
}
