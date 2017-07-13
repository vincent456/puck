package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import org.piccolo2d.util.PBounds;
import puck.piccolo2.node.PiccoloCustomNode;

import java.util.*;

/**
 * Created by Vincent Hudry on 05/06/2017.
 */
public class ArrowNodesHolder extends PNode{
    private Collection<Parrow> hiddenArrows;
    public ArrowNodesHolder(){
        hiddenArrows=new HashSet<>();
    }
    public void addArrow(Parrow arrow){
            for (Parrow arr : getAllArrows()) {
                if (arr.equals(arrow))
                    removeArrow(arr);
            }
            addChild(arrow);
    }
    public void removeArrow(Parrow arrow){
        removeChild(arrow);
    }
    public void hideArrow(Parrow arrow){
        hiddenArrows.add(arrow);
        removeArrow(arrow);
    }
    public void showArrow(Parrow arrow){
        addArrow(arrow);
        hiddenArrows.remove(arrow);
    }
    public boolean isHidden(Parrow arrow){
        return hiddenArrows.contains(arrow);
    }

    @SuppressWarnings("unchecked")
    public Collection<Parrow> getVisibleArrows(){
        Collection<Parrow> set = new HashSet<>();
        for(Iterator<PNode> iterator=getChildrenIterator();iterator.hasNext();) {
            PNode n=iterator.next();
            if(n instanceof Parrow)
            set.add((Parrow) n);
        }
        return set;
    }
    public Collection<Parrow> getHiddenArrows(){
        Collection<Parrow> set = new HashSet<>();
        for(Iterator<Parrow> iterator=hiddenArrows.iterator();iterator.hasNext();)
            set.add(iterator.next());
        return set;
    }

    public Collection<Parrow>getAllArrows(){
        Collection<Parrow> set = new HashSet<>();
        for(Iterator<PNode> iterator=getChildrenIterator();iterator.hasNext();) {
         PNode n=iterator.next();
            if(n instanceof Parrow)
            set.add((Parrow) n);
        }
        for(Iterator<Parrow> iterator=hiddenArrows.iterator();iterator.hasNext();)
            set.add(iterator.next());
        return set;
    }

    public void updatePosition(Parrow arrow) {
            PNode from = arrow.getFrom();
            PNode to = arrow.getTo();
            Parrow ar2 = arrow.redraw();
            removeArrow(arrow);
            addArrow(ar2);
    }

    public void hide_show_arrows(PiccoloCustomNode node) {
        Collection<PiccoloCustomNode> hierarchy=node.getHierarchy();
        for (PiccoloCustomNode PCN : hierarchy) {
            if (PCN.isHidden())
                for (Parrow arrow : getVisibleArrows()) {

                    PiccoloCustomNode PCNF = (PiccoloCustomNode) arrow.getFrom().getParent();
                    PiccoloCustomNode PCNT = (PiccoloCustomNode) arrow.getTo().getParent();

                    if (PCN == PCNF || PCN == PCNT) {
                        hideArrow(arrow);
                   // System.out.println("hide "+getVisibleArrows().size()+"-"+getHiddenArrows().size());
                    }
                }
            else {
                for (Parrow arrow : getHiddenArrows()) {
                    PiccoloCustomNode PCNF = (PiccoloCustomNode) arrow.getFrom().getParent();
                    PiccoloCustomNode PCNT = (PiccoloCustomNode) arrow.getTo().getParent();
                    if ((!PCNT.isHidden() || !PCNF.isHidden())
                            && (PCN == PCNF || PCN == PCNT)) {
                    showArrow(arrow);
                   // System.out.println("show "+getVisibleArrows().size()+"-"+getHiddenArrows().size());
                    }
                }
            }
        }
    }

    @SuppressWarnings("unckecked")
    public void clearCounters(){
        Collection<PNode> out=new HashSet<>();
        for(Iterator<PNode> pNodeIterator=getChildrenIterator();pNodeIterator.hasNext();){
            PNode node=pNodeIterator.next();
            if(node instanceof ArrowCounter){
                out.add(node);
            }
        }
        for(PNode node:out){
            removeChild(node);
        }
    }

    private int margin=10;

    public void updateCount(ParrowDottedFat parrow) {
        int i=0;
            for(Parrow parrow1:getVisibleArrows())
                if (parrow1 instanceof ParrowDottedFat
                        && parrow.getFrom() == parrow1.getFrom()
                        && parrow.getTo() == parrow1.getTo())
                i++;
        ArrowCounter AC=new ArrowCounter(String.valueOf(i));
        addChild(AC);
        PBounds bounds=parrow.getUnionOfChildrenBounds(null);
        parrow.setBounds(bounds);
        AC.translate(parrow.getBounds().getCenterX()+margin,parrow.getBounds().getCenterY()+margin);

    }
}
