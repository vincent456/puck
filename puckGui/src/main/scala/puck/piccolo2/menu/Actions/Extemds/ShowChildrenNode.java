package puck.piccolo2.menu.Actions.Extemds;

import org.piccolo2d.PNode;
import org.piccolo2d.event.PInputEvent;
import puck.graph.DependencyGraph;
import puck.piccolo2.Parrows.ParrowExtends;
import puck.piccolo2.menu.Actions.ShowNodeUsersOf;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.piccolo2.Parrows.ParrowFat;
import puck.piccolo2.Parrows.Triangle;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import java.awt.*;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 08/06/2017.
 */
public class ShowChildrenNode extends ShowNodeUsersOf {

    public ShowChildrenNode(DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object, PiccoloCustomNode> idNodeMap) {
        super(DG, arrowNodesHolder, idNodeMap);
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        int nodeId=target.getidNode();

        Set<Object> children=DG.subTypes(nodeId);
        for(Iterator<Object> iterator = children.toIterator(); iterator.hasNext();) {
            Object O=iterator.next();
            PNode from=idNodeMap.get(O).getContent();
            PNode to=target.getContent();
            if((from.getParent().getParent() instanceof PiccoloCustomNode)&&(!((PiccoloCustomNode) from.getParent().getParent()).isHidden()))
            arrowNodesHolder.addArrow(new ParrowExtends(from,to));
        }
    }

}
