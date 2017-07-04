package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import puck.piccolo2.node.PiccoloCustomNode;

import java.awt.*;
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
        for(Parrow arr:getAllArrows()){
            if(arr.equals(arrow))
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
        for(Iterator<PNode> iterator=getChildrenIterator();iterator.hasNext();)
            set.add((Parrow) iterator.next());
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
        for(Iterator<PNode> iterator=getChildrenIterator();iterator.hasNext();)
            set.add((Parrow) iterator.next());
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
}
