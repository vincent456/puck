package puck.piccolo2.menu;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import org.piccolo2d.nodes.PText;
import puck.piccolo2.menu.Actions.MenuItemEventHandler;

import java.awt.*;

/**
 * Created by Vincent Hudry on 01/06/2017.
 */
public class MenuItem extends PNode {
    private PPath rect;
    private PText text;

    //region getters/setters
        public double getHeight(){
            return rect.getHeight();
        }
    //endregion

    public MenuItem(String name,MenuItemEventHandler e){
        text=new PText(name);
        rect=PPath.createRectangle(0,0,text.getWidth(),text.getHeight());
        rect.setPaint(Color.GRAY);
        addChild(rect);
        addChild(text);
        this.addInputEventListener(e);
    }
    public void draw(){
        addChild(rect);
        addChild(text);
    }

    @Override
    public String toString(){
        return text.getText();
    }

}
