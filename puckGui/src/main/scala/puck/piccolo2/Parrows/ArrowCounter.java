package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import org.piccolo2d.nodes.PText;

import java.awt.*;

/**
 * Created by Vincent Hudry on 07/07/2017.
 */
public class ArrowCounter extends PNode {
    private PText text;
    private PPath rect;
    private int margin=10;
    public ArrowCounter(String text){
        this.text=new PText(text);
        rect=PPath.createRectangle(-margin,-margin,this.text.getWidth()+2*margin,this.text.getHeight()+2*margin);
        rect.setPaint(Color.WHITE);
        addChild(rect);
        addChild(this.text);
    }
}
