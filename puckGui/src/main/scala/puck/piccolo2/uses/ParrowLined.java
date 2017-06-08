package puck.piccolo2.uses;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import puck.piccolo2.Util;

import java.awt.*;
import java.awt.geom.Point2D;

public class ParrowLined extends Parrow{
    public ParrowLined(Point2D from, Point2D to, PNode head){
        super(from,to,head);
        double theta;
        theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90);
        head.translate(to.getX(),to.getY());
        head.rotate(theta);
        addChild(head);

        addChild(PPath.createLine(from.getX(),from.getY(),to.getX(),to.getY()));

    }


    protected PNode from;
    protected PNode to;

    public PNode getFrom() {
        return from;
    }

    public PNode getTo() {
        return to;
    }

    public ParrowLined(PNode from, PNode to, PNode head){
        this(from.getBounds().getCenter2D(),to.getBounds().getCenter2D(),head);
        this.from=from;
        this.to=to;
    }

}
