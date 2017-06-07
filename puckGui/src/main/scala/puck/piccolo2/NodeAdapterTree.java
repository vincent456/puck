package puck.piccolo2;

import org.piccolo2d.nodes.PImage;
import org.piccolo2d.nodes.PText;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.piccolo2.node.NodeContent;
import puck.piccolo2.node.Tree;
import puck.view.NodeKindIcons;
import scala.Tuple2;
import scala.collection.Iterator;
import scala.collection.immutable.Map;
import scala.collection.immutable.Set;

import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;

/**
 * Created by Vincent Hudry on 31/05/2017.
 */
public class NodeAdapterTree implements Tree {

    private PuckControl control;
    private int nodeId;

    public int getNodeId() {
        return nodeId;
    }

    private Collection<NodeAdapterTree> children;

    private NodeKindIcons icons;
    private DependencyGraph DG;

    public NodeAdapterTree(PuckControl control, int nodeId,NodeKindIcons icons){
        this.children=new ArrayList<>();
        this.control=control;
        this.nodeId=nodeId;

        this.icons=icons;
        DG=control.graph();

        Set<Object> childrenObjects = control.graph().content(nodeId);
        for (Iterator<Object> iterator=childrenObjects.toIterator();iterator.hasNext();){
            NodeAdapterTree NTA=new NodeAdapterTree(control,(int)iterator.next(),icons);
            this.children.add(NTA);
        }
    }

    @Override
    public Collection<Tree> getChildren() {
        return children.stream().map(x->(Tree)x).collect(Collectors.toList());
    }

    @Override
    public NodeContent getContent() {
        /*
        PNode out=new PNode();
        PText text=new PText(toString());
        PImage icon=new PImage(icons.iconOfKind(DG.getNode(nodeId).kind()).getImage());
        out.addChild(text);
        out.addChild(icon);
        text.translate(icon.getBounds().getWidth()+10,0);
        PBounds bounds=out.getUnionOfChildrenBounds(null);
        out.setBounds(bounds.getX(),bounds.getY(),bounds.getWidth(),bounds.getHeight());
        return out;
        */
        return new NodeContent(new PText(toString()),new PImage(icons.iconOfKind(DG.getNode(nodeId).kind()).getImage()));

    }

    @Override
    public String toString(){
        Map<String,Object> map=control.dg2ast().nodesByName();
        //optimize search map for String by Object
        for(Iterator<Tuple2<String, Object>> iterator = map.toIterator(); iterator.hasNext();){
            Tuple2 entry=iterator.next();
            if((int)entry._2()==nodeId){
                StringBuilder sb=new StringBuilder();
                String entrylast=DG.getNode(nodeId).name();
                sb.append(nodeId);
                sb.append("-");
                sb.append(entrylast);
                return sb.toString();
            }
        }
        return "Error";
    }

}
