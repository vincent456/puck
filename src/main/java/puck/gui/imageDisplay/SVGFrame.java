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
    public SVGFrame(InputStream stream, DependencyGraph g) throws IOException {
        SVGController controller = new SVGController();
        controller.pushGraph(g);
        SVGPanel panel = SVGPanel.fromStream(stream, controller);
        this.add(panel);
        this.setVisible(true);
        this.setMinimumSize(new Dimension(640,480));
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    }
}
