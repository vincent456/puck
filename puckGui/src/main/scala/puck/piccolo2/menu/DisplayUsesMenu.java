package puck.piccolo2.menu;

import org.piccolo2d.PNode;
import puck.graph.DependencyGraph;
import puck.piccolo2.menu.Actions.CloseMenu;
import puck.piccolo2.menu.Actions.ShowNodeUses;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.uses.ArrowNodesHolder;

import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class DisplayUsesMenu extends PNode {

    private Menu menu;

    //region for showusedby

    private int nodeId=-1;
    private DependencyGraph DG;
    private ArrowNodesHolder ANH;
    private HashMap<Object,PiccoloCustomNode> idNodeMap;
    private PNode target;
    ShowNodeUses SNU;

    //endregion

    public DisplayUsesMenu(DependencyGraph DG, ArrowNodesHolder ANH,HashMap<Object,PiccoloCustomNode> idNodeMap){
        menu=new Menu();

        MenuItem closes=new MenuItem("Close Menu",new CloseMenu(this));
        menu.add(closes);

        //region for showusedby

        this.DG=DG;
        this.ANH=ANH;
        this.idNodeMap=idNodeMap;
        SNU=new ShowNodeUses(DG,ANH,idNodeMap);
        MenuItem showusedby=new MenuItem("Show users of",SNU);
        menu.add(showusedby);

        //endregion
    }
    public void draw(Point2D position){
        clear();
        addChild(menu);
        menu.draw(position);
    }
    public void clear(){
        try {
            AffineTransform t= menu.getTransform().createInverse();
            menu.translate(t.getTranslateX(), t.getTranslateY());
        }
        catch (Exception e){
            System.err.println(e.getMessage());
        }
        removeAllChildren();
    }

    public void setTarget(PNode target){
        this.target=target;
        SNU.setTarget((PiccoloCustomNode)target);
        menu.setTarget(target);
    }
}
