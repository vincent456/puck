package puck.gui.svg;

import puck.graph.DependencyGraph;
import puck.graph.io.PrintingOptions;
import puck.gui.PuckControl;
import puck.gui.svg.SVGController;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;
import java.io.InputStream;

/**
 * Created by lorilan on 3/13/15.
 */


public class SVGFrame extends JFrame{


    static class SVGConsole {
        private JTextArea console = new JTextArea();
        {
            console.setEditable(false);
        }
        void selectedNode(String node){
            if(node.length()>0)
                console.setText("Node selected : " + node);
            else
                console.setText("No node selected");
        }
        void setText(String txt) {
            console.setText(txt);
        }
        void appendText(String txt) {
            console.append(txt);
        }
    }


    public SVGFrame(InputStream stream,
                    DependencyGraph g,
                    PrintingOptions opts,
                    PuckControl control) throws IOException {
        this.setLayout(new BorderLayout());
        SVGPanel panel = new SVGPanel(SVGController.documentFromStream(stream));

        SVGConsole console = new SVGConsole();

        SVGController controller = SVGController.apply(control, g, opts, panel.canvas, console);
        panel.setController(controller);

        this.add(panel, BorderLayout.CENTER);
        this.add(console.console, BorderLayout.SOUTH);

        this.setVisible(true);
        this.setMinimumSize(new Dimension(640, 480));
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    }
}
