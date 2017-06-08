package puck.piccolo2.menu.Actions.Extemds;

import org.piccolo2d.event.PInputEvent;
import puck.graph.DependencyGraph;
import puck.piccolo2.menu.Actions.ShowNodeUsersOf;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.uses.ArrowNodesHolder;
import puck.piccolo2.uses.Parrow;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import java.util.Collection;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 08/06/2017.
 */
public class HideChildrenNode extends ShowNodeUsersOf {
    public HideChildrenNode(DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object, PiccoloCustomNode> idNodeMap) {
        super(DG, arrowNodesHolder, idNodeMap);
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        int nodeId=target.getidNode();

        Set<Object> parent=DG.subTypes(nodeId);
        Collection<Parrow> quiver=arrowNodesHolder.getVisibleArrows();
        for(Parrow A:quiver){
            PiccoloCustomNode from= (PiccoloCustomNode) A.getFrom().getParent();
            PiccoloCustomNode to= (PiccoloCustomNode) A.getTo().getParent();
            for(Iterator<Object> iterator = parent.iterator(); iterator.hasNext();)
                if(from.getidNode()==(int)iterator.next()){
                    arrowNodesHolder.removeArrow(A);
                }
        }

    }
}
