package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

/**
 * Created by Vincent Hudry on 05/06/2017.
 */
public class ArrowNodesHolder extends PNode{
    private Collection<Parrow> hiddenArrows;
    public ArrowNodesHolder(){
        hiddenArrows=new HashSet<>();
    }
    public void addArrow(Parrow arrow){
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
}
