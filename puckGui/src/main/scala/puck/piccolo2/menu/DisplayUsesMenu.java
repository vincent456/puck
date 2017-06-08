package puck.piccolo2.menu;

import org.piccolo2d.PNode;
import puck.graph.DependencyGraph;
import puck.piccolo2.menu.Actions.*;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.uses.ArrowNodesHolder;

import java.awt.geom.AffineTransform;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class DisplayUsesMenu extends PNode {

    private Menu menu;

    //region for showusedby

    //endregion

    public DisplayUsesMenu(DependencyGraph DG, ArrowNodesHolder ANH,HashMap<Object,PiccoloCustomNode> idNodeMap){
        menu=new Menu();

        MenuItem closes=new MenuItem("Close Menu",new CloseMenu(this));
        menu.add(closes);

        //region for showuses

        ShowNodeUsersOf SNUO =new ShowNodeUsersOf(DG,ANH,idNodeMap);
        MenuItem showusersof=new MenuItem("Show users of", SNUO);
        menu.add(showusersof);

        HideNodeUsersOf HNUO=new HideNodeUsersOf(DG,ANH,idNodeMap);
        MenuItem hideusersof=new MenuItem("Hide users of",HNUO);
        menu.add(hideusersof);

        ShowNodeUsedBy SNOB = new ShowNodeUsedBy(DG,ANH,idNodeMap);
        MenuItem shownodeusedby=new MenuItem("Show used by",SNOB);
        menu.add(shownodeusedby);

        HideNodeUsedBy HNUB=new HideNodeUsedBy(DG,ANH,idNodeMap);
        MenuItem hidenodeusedby=new MenuItem("Hide used by",HNUB);
        menu.add(hidenodeusedby);

        //endregion
    }
    public void draw(PNode target){
        clear();
        addChild(menu);
        menu.draw(target);
    }
    public void clear(){
        menu.setTransform(AffineTransform.getTranslateInstance(0,0));

        removeAllChildren();
    }
}
