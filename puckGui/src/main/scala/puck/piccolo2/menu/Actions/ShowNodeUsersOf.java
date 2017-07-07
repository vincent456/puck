package puck.piccolo2.menu.Actions;

import org.piccolo2d.PNode;
import org.piccolo2d.event.PInputEvent;
import puck.control.Node;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.piccolo2.node.NodeContent;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.*;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import java.awt.*;
import java.util.Collection;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class ShowNodeUsersOf extends MenuItemEventHandler {

    protected DependencyGraph DG;
    protected ArrowNodesHolder arrowNodesHolder;
    protected HashMap<Object,PiccoloCustomNode> idNodeMap;

    protected PuckControl control;

    public ShowNodeUsersOf(PuckControl control,DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object,PiccoloCustomNode> idNodeMap){
        this.DG=DG;
        this.arrowNodesHolder=arrowNodesHolder;
        this.idNodeMap=idNodeMap;
        this.control=control;
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        int nodeId=target.getidNode();

        Set<Object> usersof=DG.usersOf(nodeId);
        //region for target node
        for(Iterator<Object> iterator = usersof.toIterator(); iterator.hasNext();) {
            Object O=iterator.next();
            PNode from=target.getContent();
            PNode to=idNodeMap.get(O).getContent();
            if(to.getParent() instanceof  PiccoloCustomNode &&!((PiccoloCustomNode)to.getParent()).isHidden()){

                int user = (int)O;
                int used = target.getidNode();

                boolean forbidden = control.constraints().get().isForbidden(DG,user,used);

                if(forbidden)
                    arrowNodesHolder.addArrow(new ParrowFat(from,to,5, Color.RED));
                else
                    arrowNodesHolder.addArrow(new ParrowUses(from, to, 10));
            }
            else {
                if(to.getParent() instanceof  PiccoloCustomNode){

                    int user=(int)O;
                    int used= target.getidNode();

                    boolean forbidden=control.constraints().get().isForbidden(DG,user,used);

                    if(forbidden) {
                        NodeContent virtualTo=((PiccoloCustomNode)to.getParent()).getHigherParent().getContent();
                        ParrowDottedFat arrow=new ParrowDottedFat(from,virtualTo,10,5,Color.RED,from,to);
                        arrowNodesHolder.addArrow(arrow);
                    }
                }
            }
        }
        //endregion

        //region for hierarchy nodes
        Collection<PiccoloCustomNode> hierarchy1 = target.getHierarchy();
        Collection<PiccoloCustomNode> hierarchy2 = target.getHierarchy();

        for(PiccoloCustomNode PCN1:hierarchy1)
            for(PiccoloCustomNode PCN2:hierarchy2){
                int nodeId1=PCN1.getidNode();
                int nodeId2=PCN2.getidNode();
                if(PCN1.isHidden()&&PCN2.isHidden())
                if(DG.usersOf(nodeId1).contains(nodeId2)){
                    if(control.constraints().get().isForbidden(DG,nodeId2,nodeId1)){
                        arrowNodesHolder.addArrow(new ParrowDottedFat(PCN1.getHigherParent().getContent(),PCN2.getHigherParent().getContent(),10,5,Color.RED,PCN1.getContent(),PCN2.getContent()));
                    }
                }
            }
        //endregion
    }
}
