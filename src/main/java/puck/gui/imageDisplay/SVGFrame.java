package puck.gui.imageDisplay;

import puck.graph.DependencyGraph;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;
import java.io.InputStream;

/**
 * Created by lorilan on 3/13/15.
 */
public class SVGFrame extends JFrame{
    private SVGController controller = new SVGController();
    public SVGFrame(InputStream stream, DependencyGraph g) throws IOException {
        controller.pushGraph(g);
        SVGPanel panel = SVGPanel.fromStream(stream, controller);
        this.add(panel);
        this.setVisible(true);
        this.setMinimumSize(new Dimension(640,480));
    }
}
