package puck.piccolo2.node;

import org.piccolo2d.PNode;

import java.util.Collection;

public interface Tree {
    Collection<Tree> getChildren();
    PNode getContent();
    int getNodeId();
}
