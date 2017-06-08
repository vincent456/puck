package puck.piccolo2.uses;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import puck.piccolo2.Util;
import puck.piccolo2.node.NodeContent;

import java.awt.*;
import java.awt.geom.Point2D;

public class Parrow extends PNode{
    public Parrow(Point2D from, Point2D to, PNode head, float width, Color color){

        double dist= Util.distance(from.getX(),from.getY(),to.getX(),to.getY());
        PPath rect=PPath.createRectangle(0,0,width,dist-head.getBounds().height/2);
        rect.setPaint(color);
        double theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90+180);
        rect.rotate(theta);
        rect.translate(-width/2,0);
        rect.translate(from.getX(),from.getY());
        //TODO fix translation problem on constructor with node parameters
        //addChild(rect);

        theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90);
        head.translate(to.getX(),to.getY());
        head.rotate(theta);
        addChild(head);

        addChild(PPath.createLine(from.getX(),from.getY(),to.getX(),to.getY()));

    }


    private PNode from;
    private PNode to;

    public PNode getFrom() {
        return from;
    }

    public PNode getTo() {
        return to;
    }

    public Parrow(PNode from,PNode to, PNode head, float width, Color color){
        this(new Point2D.Double(from.getBounds().getCenter2D().getX(),from.getBounds().getCenter2D().getY()),new Point2D.Double(to.getBounds().getCenter2D().getX(),to.getBounds().getCenter2D().getY()),head,width,color);
        this.from=from;
        this.to=to;
    }

}
