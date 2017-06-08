package puck.piccolo2.node;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PImage;
import org.piccolo2d.nodes.PText;
import org.piccolo2d.util.PBounds;

import java.awt.geom.Point2D;

/**
 * Created by Vincent Hudry on 07/06/2017.
 */
public class NodeContent extends PNode {
    private PText text;
    private PImage icon;
    private int margin;
    private Point2D Position;

    //region getters/setters
    public int getMargin() {
        return margin;
    }

    public void setMargin(int margin) {
        this.margin = margin;
    }

    public Point2D getPosition() {
        return Position;
    }

    public void updatePosition(Point2D position) {
        Position = position;
    }

    public PText getText(){
        return text;
    }

    public PImage getIcon(){
        return icon;
    }

    //endregion
    
    public NodeContent(PText text, PImage Icon){
        this.text=text;
        this.icon=Icon;
        addChild(this.text);
        addChild(Icon);
        text.setBounds(Icon.getBounds().getWidth()+margin,0,text.getWidth(),text.getHeight());
        PBounds bounds=this.getUnionOfChildrenBounds(null);
        this.setBounds(bounds.getX(),bounds.getY(),bounds.getWidth(),bounds.getHeight());
    }
}
