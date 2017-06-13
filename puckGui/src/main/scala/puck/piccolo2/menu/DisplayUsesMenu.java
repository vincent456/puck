package puck.piccolo2.menu;

import org.piccolo2d.PNode;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.graph.NodeKind;
import puck.graph.transformations.TransformationRules;
import puck.piccolo2.menu.Actions.*;
import puck.piccolo2.menu.Actions.Actions2.AddChildKind;
import puck.piccolo2.menu.Actions.Actions2.Rename;
import puck.piccolo2.menu.Actions.Extemds.HideChildrenNode;
import puck.piccolo2.menu.Actions.Extemds.HideParentNode;
import puck.piccolo2.menu.Actions.Extemds.ShowChildrenNode;
import puck.piccolo2.menu.Actions.Extemds.ShowParentNode;
import puck.piccolo2.menu.Actions.Uses.HideNodeUsedBy;
import puck.piccolo2.menu.Actions.Uses.HideNodeUsersOf;
import puck.piccolo2.menu.Actions.Uses.ShowNodeUsedBy;
import puck.piccolo2.menu.Actions.ShowNodeUsersOf;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.ArrowNodesHolder;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.util.*;
import java.util.List;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class DisplayUsesMenu extends PNode {

    private Menu menu;

    private PuckControl control;

    private HashMap<Object,PiccoloCustomNode> idNodeMap;

    public DisplayUsesMenu(PuckControl control, ArrowNodesHolder ANH, HashMap<Object,PiccoloCustomNode> idNodeMap){
        menu=new Menu();

        MenuItem closes=new MenuItem("Close Menu",new CloseMenu(this), Color.GRAY);
        menu.add(closes);

        DependencyGraph DG=control.graph();
        TransformationRules TR=control.graphUtils().Rules();

        //region for showuses

        ShowNodeUsersOf SNUO =new ShowNodeUsersOf(DG,ANH,idNodeMap);
        MenuItem showusersof=new MenuItem("Show users of", SNUO,Color.YELLOW);
        menu.add(showusersof);

        HideNodeUsersOf HNUO=new HideNodeUsersOf(DG,ANH,idNodeMap);
        MenuItem hideusersof=new MenuItem("Hide users of",HNUO,Color.YELLOW);
        menu.add(hideusersof);

        ShowNodeUsedBy SNOB = new ShowNodeUsedBy(DG,ANH,idNodeMap);
        MenuItem shownodeusedby=new MenuItem("Show used by",SNOB,Color.YELLOW);
        menu.add(shownodeusedby);

        HideNodeUsedBy HNUB=new HideNodeUsedBy(DG,ANH,idNodeMap);
        MenuItem hidenodeusedby=new MenuItem("Hide used by",HNUB,Color.YELLOW);
        menu.add(hidenodeusedby);

        //endregion

        //region for Extends
        ShowParentNode SPN=new ShowParentNode(DG,ANH,idNodeMap);
        MenuItem showparentnode=new MenuItem("Show parents",SPN,Color.RED);
        menu.add(showparentnode);

        HideParentNode HPN=new HideParentNode(DG,ANH,idNodeMap);
        MenuItem hideparentnode=new MenuItem("Hide parents",HPN,Color.RED);
        menu.add(hideparentnode);

        ShowChildrenNode SCN=new ShowChildrenNode(DG,ANH,idNodeMap);
        MenuItem showchildrennode=new MenuItem("Show children",SCN,Color.RED);
        menu.add(showchildrennode);

        HideChildrenNode HCN=new HideChildrenNode(DG,ANH,idNodeMap);
        MenuItem hidechildrennode=new MenuItem("Hide children",HCN,Color.RED);
        menu.add(hidechildrennode);

        //endregion

        //region rename

        Rename rename=new Rename(control,idNodeMap);
        MenuItem renam=new MenuItem("Rename",rename,Color.GRAY);
        menu.add(renam);

        //endregion

        //region AddChildKind

        this.control=control;
        dynamicItems=new LinkedList<>();

        this.idNodeMap=idNodeMap;

        //endregion

    }

    private List<MenuItem> dynamicItems;

    public void draw(PNode target){
        for(MenuItem item:dynamicItems){
            menu.remove(item);
        }

        dynamicItems=new LinkedList<>();

        clear();
        addChild(menu);

        //region AddChildKind
        PiccoloCustomNode PCN= (PiccoloCustomNode) target.getParent().getParent();
        DependencyGraph DG = control.graph();
        //java.util.List<NodeKind> list=new LinkedList<>();
        for(scala.collection.Iterator<NodeKind> iterator = DG.nodeKinds().iterator(); iterator.hasNext();){
            NodeKind nk=iterator.next();
            if(DG.getNode(PCN.getidNode()).kind().canContain(nk)){
                //list.add(nk);
                AddChildKind ACK=new AddChildKind(control,nk,idNodeMap.get(0));

                MenuItem menuItem=new MenuItem("Add "+ nk.toString(),ACK,Color.GRAY);
                    dynamicItems.add(menuItem);
                    menu.add(menuItem);
                }
        }

        //endregion

        menu.draw(target);
    }
    public void clear(){
        menu.setTransform(AffineTransform.getTranslateInstance(0,0));

        removeAllChildren();
    }
}
