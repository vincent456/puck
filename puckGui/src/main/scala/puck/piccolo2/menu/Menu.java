package puck.piccolo2.menu;

import org.piccolo2d.PNode;
import puck.piccolo2.node.PiccoloCustomNode;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by Vincent Hudry on 01/06/2017.
 */
public class Menu extends PNode {

    private List<MenuItem> items;

    private PNode target;

    public Menu(){
        items=new ArrayList<>();
    }

    //region getters/setters
    public void setTarget(PNode target){
        this.target=target;
        PiccoloCustomNode PCN=(PiccoloCustomNode) target.getParent().getParent();
        
    }
    //endregion

    public void add(MenuItem item){
        items.add(item);
    }

    public void draw(Point2D position){

        double x=position.getX();
        double y = position.getY();

        for(MenuItem m:items) {
            try {
                m.transformBy(m.getTransform().createInverse());
            }
            catch (Exception e){
                System.err.println("Menu 33" + e.getMessage());
            }
            m.translate(x,y);
            addChild(m);
            y+=m.getHeight();
        }
    }

}
