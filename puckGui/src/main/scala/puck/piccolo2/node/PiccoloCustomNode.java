package puck.piccolo2.node;

import org.piccolo2d.PCanvas;
import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import org.piccolo2d.nodes.PText;
import puck.piccolo2.PiccoloCanvas;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

public class PiccoloCustomNode extends PNode {
    private NodeContent content;
    private PPath rect;

    private Collection<PiccoloCustomNode> hiddenchildren;
    private double margin=10;

    private int idNode;

    //region getters/setters

    public PPath getRect() {
        return rect;
    }

    public NodeContent getContent() {
        return content;
    }

    public double getMargin() {
        return margin;
    }

    public void setMargin(double margin) {
        this.margin = margin;
    }

    public int getidNode(){
        return idNode;
    }

    public String toString(){
        return ((PText)(content.getChild(0))).getText();
    }

    public void setLayout(){setGridLayoutV();}

    //endregion


    public PiccoloCustomNode(Tree tree){

        this.idNode=tree.getNodeId();

        hiddenchildren=new ArrayList<>();
        content=tree.getContent();

        double width=margin+content.getBounds().getWidth()+margin;
        double height=margin+content.getBounds().getHeight()+margin;

        rect=PPath.createRectangle(0,0,width,height);
        addChild(rect);
        addChild(content);
        content.translate(margin,margin);

        if(tree.getChildren()!=null)
        for(Tree T:tree.getChildren()) {
            PiccoloCustomNode node = new PiccoloCustomNode(T);
            hiddenchildren.add(node);
        }
    }

    private PiccoloCustomNode(){}

    public void addChildNode(Tree t){
        PiccoloCustomNode PCN=new PiccoloCustomNode(t);

        PCN.getContent().addInputEventListener(new PCustomInputEventHandler(PCN, PiccoloCanvas.getRoot(),PiccoloCanvas.getMenu(),PiccoloCanvas.getCanvas()));

        boolean isHiddingChildren=hiddenchildren.size()!=0;
        if(isHiddingChildren)
            hiddenchildren.add(PCN);
        else
            addChild(PCN);
    }

    public Collection<PiccoloCustomNode> getChildren(){
        ArrayList<PiccoloCustomNode> children=new ArrayList<>();
        for(Iterator<PNode> childrenIterator=getChildrenIterator();childrenIterator.hasNext();){
            PNode child = childrenIterator.next();
            if(child instanceof PiccoloCustomNode){
                children.add((PiccoloCustomNode) child);
            }
        }
        return children;
    }

    public Collection<PiccoloCustomNode> getAllChildren(){
        Collection<PiccoloCustomNode> out=new ArrayList<>();
        Collection<PiccoloCustomNode> children=getChildren();
        for(PiccoloCustomNode PCN:children)
            out.add(PCN);
        for (PiccoloCustomNode PCN:hiddenchildren)
            out.add(PCN);
        return out;
    }

    public void toggleChildren(){
        
        Collection<PiccoloCustomNode> children=getChildren();
        if(children.size()!=0){
            for(PiccoloCustomNode PCN:children)
                hiddenchildren.add(PCN);
            removeAllChildren();

            addChild(rect);
            addChild(content);
        }
        else {
            for (PiccoloCustomNode PCN:hiddenchildren)
                addChild(PCN);
            hiddenchildren.clear();
        }

    }

    //linear horizontal layout
    public void setGridLayoutH(){
        if(getChildren().size()==0){

           double x=0;
           double y=0;
           double w=margin+content.getBounds().getWidth()+margin;
           double h=margin+content.getBounds().getHeight()+margin;

           removeChild(rect);
           rect=PPath.createRectangle(x,y,w,h);
           rect=bevelOut(rect,2);
           addChild(rect);
           addChild(content);

            return;
        }

        NodeContent content=this.content;
        Collection<PiccoloCustomNode> children=getChildren();

        double x=margin;
        double y=margin+content.getBounds().getHeight()+margin;
        double w=margin+content.getBounds().getWidth()+margin;
        double h=margin+content.getBounds().getHeight()+margin;

        PiccoloCustomNode lastChild=null;

        //region horizontal layout
        for(PiccoloCustomNode PCN:children){
            lastChild=PCN;

            PCN.setTransform(AffineTransform.getTranslateInstance(0,0));

            PCN.setGridLayoutH();

            PCN.translate(x,y);

            x+=PCN.getRect().getWidth()+margin;
            w+=PCN.getRect().getWidth()+margin;
        }

        double maxHeight=lastChild.getRect().getHeight();
        for(PiccoloCustomNode PCN:children)
            if(PCN.getRect().getHeight()>maxHeight)
                maxHeight=PCN.getRect().getHeight();
        h+=maxHeight+margin;


        //endregion

        removeChild(rect);
        rect=PPath.createRectangle(0,0,w,h);
        rect=bevelIn(rect,2);
        addChild(rect);
        addChild(content);
        addChildren(children);
    }

    //linear vertical layout
    public void setGridLayoutV(){
        if(getChildren().size()==0){

            double x=0;
            double y=0;
            double w=margin+content.getBounds().getWidth()+margin;
            double h=margin+content.getBounds().getHeight()+margin;

            removeChild(rect);
            rect=PPath.createRectangle(x,y,w,h);
            rect=bevelOut(rect,2);
            addChild(rect);
            addChild(content);

            return;
        }

        PNode content=this.content;
        Collection<PiccoloCustomNode> children=getChildren();

        double x=margin+content.getBounds().getWidth()+margin;
        double y=0;
        double w=margin+content.getBounds().getWidth()+margin;
        double h=margin+content.getBounds().getHeight()+margin;

        PiccoloCustomNode lastChild=null;

        //region vertical layout
        for(PiccoloCustomNode PCN:children){
            lastChild=PCN;

            PCN.setTransform(AffineTransform.getTranslateInstance(0,0));

            PCN.setGridLayoutV();

            PCN.translate(x,y);

            y+=PCN.getRect().getHeight()+margin;
            h+=PCN.getRect().getHeight()+margin;
        }

        double maxWidth=lastChild.getRect().getWidth();
        for(PiccoloCustomNode PCN:children)
            if(PCN.getRect().getWidth()>maxWidth)
                maxWidth=PCN.getRect().getWidth();
        w+=maxWidth+margin;


        //endregion

        removeChild(rect);
        rect=PPath.createRectangle(0,0,w,h);
        rect=bevelIn(rect,2);
        addChild(rect);
        addChild(content);
        addChildren(children);
    }

    public PPath bevelOut(PPath rectangle,int bevel){
        double w=rectangle.getWidth();
        double h=rectangle.getHeight();

        PPath background = PPath.createRectangle(0,0,w,h);
        background.setPaint(Color.WHITE);
        PPath borderTop=PPath.createRectangle(0,0,w,bevel);
        borderTop.setPaint(Color.LIGHT_GRAY);
        borderTop.setStroke(null);
        background.addChild(borderTop);
        PPath borderLeft=PPath.createRectangle(0,0,bevel,h);
        borderLeft.setPaint(Color.LIGHT_GRAY);
        borderLeft.setStroke(null);
        background.addChild(borderLeft);
        PPath borderRight=PPath.createRectangle(w-bevel,0,bevel,h);
        borderRight.setPaint(Color.DARK_GRAY);
        borderRight.setStroke(null);
        background.addChild(borderRight);
        PPath borderBottom=PPath.createRectangle(0,h-bevel,w,bevel);
        borderBottom.setPaint(Color.DARK_GRAY);
        borderBottom.setStroke(null);
        background.addChild(borderBottom);

        return background;
    }

    public PPath bevelIn(PPath rectangle,int bevel){
        double w=rectangle.getWidth();
        double h=rectangle.getHeight();

        PPath background = PPath.createRectangle(0,0,w,h);
        background.setPaint(Color.WHITE);
        PPath borderTop=PPath.createRectangle(0,0,w,bevel);
        borderTop.setPaint(Color.DARK_GRAY);
        borderTop.setStroke(null);
        background.addChild(borderTop);
        PPath borderLeft=PPath.createRectangle(0,0,bevel,h);
        borderLeft.setPaint(Color.DARK_GRAY);
        borderLeft.setStroke(null);
        background.addChild(borderLeft);
        PPath borderRight=PPath.createRectangle(w-bevel,0,bevel,h);
        borderRight.setPaint(Color.LIGHT_GRAY);
        borderRight.setStroke(null);
        background.addChild(borderRight);
        PPath borderBottom=PPath.createRectangle(0,h-bevel,w,bevel);
        borderBottom.setPaint(Color.LIGHT_GRAY);
        borderBottom.setStroke(null);
        background.addChild(borderBottom);

        return background;
    }

    public void updateContentBoundingBoxes(boolean debug, PCanvas canvas){

        updateTextBoundingBoxes(false);

        Point2D GLobalTranslation=content.getGlobalTranslation();

        double x=GLobalTranslation.getX();
        double y= GLobalTranslation.getY();
        double w=content.getBounds().getWidth();
        double h=content.getBounds().getHeight();
        content.setBounds(x,y,w,h);

        if(debug)
            canvas.getLayer().addChild(PPath.createRectangle(x,y,w,h));

        for(PiccoloCustomNode PCN:getChildren())
            PCN.updateContentBoundingBoxes(debug,canvas);
    }

    public void updateTextBoundingBoxes(boolean debug){

        Point2D globalTranslation=content.getText().getGlobalTranslation();
        Point2D contentGlobalTranslation=content.getGlobalTranslation();

        double x=globalTranslation.getX()-contentGlobalTranslation.getX();
        double y= globalTranslation.getY()-contentGlobalTranslation.getY();
        double w=content.getText().getBounds().getWidth();
        double h=content.getText().getBounds().getHeight();

        content.getText().setBounds(x+content.getIcon().getWidth()+content.getMargin(),y,w,h);

        if(debug)
            content.addChild(PPath.createRectangle(x,y,w,h));

        //for(PiccoloCustomNode PCN:getChildren())
        //    PCN.updateContentBoundingBoxes(debug,canvas);
    }

    public boolean isHidden(){
        PNode parent=getParent();
        if(parent==null)
            return false;
        if(parent instanceof PiccoloCustomNode){
            PiccoloCustomNode PCNparent=(PiccoloCustomNode) parent;
            if(PCNparent.getChildren().contains(this))
                return false;
            else
                return true;
        }
        else {
            System.err.println("error");
            return false;
        }
    }

    //TODO implement setGridLayout to display items into a grid
}
