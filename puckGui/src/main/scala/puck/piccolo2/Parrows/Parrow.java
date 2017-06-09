package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;

import java.awt.*;
import java.awt.geom.Point2D;

public abstract class Parrow extends PNode{
    public Parrow(Point2D from, Point2D to, PNode head){
    }


    protected PNode from;
    protected PNode to;

    public PNode getFrom() {
        return from;
    }

    public PNode getTo() {
        return to;
    }

    public Parrow(PNode from,PNode to, PNode head, float width, Color color){
        this(from.getBounds().getCenter2D(),to.getBounds().getCenter2D(),head);
        this.from=from;
        this.to=to;
    }

}
