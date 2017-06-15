package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import puck.piccolo2.Util;

import java.awt.*;
import java.awt.geom.Point2D;

public class ParrowExtends extends Parrow{
    public ParrowExtends(Point2D from, Point2D to){
        super(from,to);
        Triangle head=new Triangle(Color.WHITE);

        PPath line=PPath.createLine(from.getX(),from.getY(),to.getX(),to.getY());

        double theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90);
        head.translate(to.getX(),to.getY());
        head.rotate(theta);

        addChild(line);
        addChild(head);

    }

    public ParrowExtends(PNode from, PNode to){
        this(from.getBounds().getCenter2D(),to.getBounds().getCenter2D());
        this.from=from;
        this.to=to;
    }

    @Override
    public Parrow redraw() {
        removeAllChildren();
        return new ParrowExtends(from,to);
    }

}
