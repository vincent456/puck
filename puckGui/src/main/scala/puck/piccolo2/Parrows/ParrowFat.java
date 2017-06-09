package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import puck.piccolo2.Util;

import java.awt.*;
import java.awt.geom.Point2D;

public class ParrowFat extends Parrow{
    public ParrowFat(Point2D from, Point2D to, PNode head, float width, Color color){
        super(from,to,head);
        double dist= Util.distance(from.getX(),from.getY(),to.getX(),to.getY());
        PPath rect=PPath.createRectangle(0,0,width,dist-head.getBounds().height/2);
        rect.setPaint(color);
        double theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90+180);
        rect.translate(from.getX(),from.getY());
        rect.rotate(theta);
        rect.translate(-width/2,0);
        addChild(rect);

        theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90);
        head.translate(to.getX(),to.getY());
        head.rotate(theta);
        addChild(head);

    }

    public ParrowFat(PNode from, PNode to, PNode head, float width, Color color){
        this(from.getBounds().getCenter2D(),to.getBounds().getCenter2D(),head,width,color);
        this.from=from;
        this.to=to;
    }

}
