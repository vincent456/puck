package puck.gui.svg;

import puck.graph.DependencyGraph;
import puck.graph.io.PrintingOptions;
import puck.gui.PuckControl;
import puck.gui.svg.SVGController;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.InputStream;

/**
 * Created by lorilan on 3/13/15.
 */


public class SVGFrame extends JFrame implements StackListener {



    static class SVGConsole {
        private JTextArea console = new JTextArea();
        {
            console.setEditable(false);
        }
        void displaySelection(String node){
            if(node.length()>0)
                console.setText("Selection : " + node);
            else
                console.setText("No selection");
        }
        void setText(String txt) {
            console.setText(txt);
        }
        void appendText(String txt) {
            console.append(txt);
        }
    }

    @Override
    public void update(SVGController svgController) {
        undoButton.setEnabled(svgController.canUndo());
        redoButton.setEnabled(svgController.canRedo());
    }

    private JButton undoButton;
    private JButton redoButton;

    public SVGFrame(InputStream stream,
                    DependencyGraph g,
                    PrintingOptions opts,
                    PuckControl control) throws IOException {
        this.setLayout(new BorderLayout());
        SVGPanel panel = new SVGPanel(SVGController.documentFromStream(stream));

        SVGConsole console = new SVGConsole();

        final SVGController controller = SVGController.apply(control, g, opts, panel.canvas, console);
        panel.setController(controller);
        controller.registerAsStackListeners(this);

        JPanel menu = new JPanel();

        undoButton =new JButton(new AbstractAction("Undo"){
            @Override
            public void actionPerformed(ActionEvent e) {
                controller.undo();
            }
        });
        undoButton.setEnabled(false);
        menu.add(undoButton);

        redoButton =new JButton(new AbstractAction("Redo"){
            @Override
            public void actionPerformed(ActionEvent e) {
                controller.redo();
            }
        });
        redoButton.setEnabled(false);
        menu.add(redoButton);


        menu.add(console.console);


        this.add(panel, BorderLayout.CENTER);
        this.add(menu, BorderLayout.SOUTH);


        this.setVisible(true);
        this.setMinimumSize(new Dimension(640, 480));
        this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    }
}
