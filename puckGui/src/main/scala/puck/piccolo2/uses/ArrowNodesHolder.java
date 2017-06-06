package puck.piccolo2.uses;

import org.piccolo2d.PNode;

import java.util.Collection;
import java.util.HashSet;

/**
 * Created by Vincent Hudry on 05/06/2017.
 */
public class ArrowNodesHolder extends PNode {
    private Collection<PNode> hiddenArrows;
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
}
