package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import org.piccolo2d.util.PBounds;

import java.awt.geom.Point2D;

public abstract class Parrow extends PNode{
    public Parrow(Point2D from, Point2D to){
    }

    protected PNode from;
    protected PNode to;

    public PNode getFrom() {
        return from;
    }

    public PNode getTo() {
        return to;
    }

    public void setFrom(PNode from){
        this.from=from;
    }

    public void setTo(PNode to){
        this.to=to;
    }

    public Parrow(PNode from,PNode to){
        this(from.getBounds().getCenter2D(),to.getBounds().getCenter2D());
        this.from=from;
        this.to=to;
    }

    public abstract Parrow redraw();

    @Override
    public boolean equals(Object arrow){
        if(!(arrow instanceof Parrow))
            return false;
        return this.from==((Parrow) arrow).getFrom()
                &&this.to==((Parrow) arrow).getTo();
    }
}
