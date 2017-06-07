package puck.piccolo2.menu.Actions;

import org.piccolo2d.event.PInputEvent;
import puck.graph.DependencyGraph;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.uses.ArrowNodesHolder;
import puck.piccolo2.uses.Parrow;
import scala.Int;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import java.util.Collection;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class HideNodeUsersOf extends ShowNodeUsersOf {

    public HideNodeUsersOf(DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object,PiccoloCustomNode> idNodeMap){
        super(DG,arrowNodesHolder,idNodeMap);
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        int nodeId=target.getidNode();

        Set<Object> usersof=DG.usersOf(nodeId);
        Collection<Parrow> quiver=arrowNodesHolder.getVisibleArrows();
        for(Parrow A:quiver){
            PiccoloCustomNode from= (PiccoloCustomNode) A.getFrom().getParent();
            PiccoloCustomNode to= (PiccoloCustomNode) A.getTo().getParent();
            for(Iterator<Object> iterator = usersof.iterator(); iterator.hasNext();)
            if(to.getidNode()==(int)iterator.next()){
                arrowNodesHolder.removeArrow(A);
            }
        }

    }
}
