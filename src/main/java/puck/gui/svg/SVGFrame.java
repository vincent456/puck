package puck.gui.svg;

import puck.graph.DependencyGraph;
import puck.graph.io.PrintingOptions;
import puck.gui.PuckControl;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.InputStream;

/**
 * Created by lorilan on 3/13/15.
 */
public class SVGFrame extends JFrame implements StackListener {

    static public class SVGConsole {
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
        public void setText(String txt) {
            console.setText(txt);
        }
        public void appendText(String txt) {
            console.append(txt);
        }
    }

    @Override
    public void update(SVGController svgController) {
        undoButton.setEnabled(svgController.canUndo());
        redoButton.setEnabled(svgController.canRedo());
    }

    private final SVGController controller;
    private JButton undoButton;
    private JButton redoButton;

    private JPanel menu = new JPanel();


    private void addUndoRedoButtonToMenu(){
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

    }

    private void addVisibilityCheckBoxesToMenu(){
        final JCheckBox sigCheckBox = new JCheckBox();
        sigCheckBox.setAction(new AbstractAction("Show signatures") {
            @Override
            public void actionPerformed(ActionEvent e) {
                controller.setSignatureVisible(sigCheckBox.isSelected());
            }
        });
        menu.add(sigCheckBox);

        final JCheckBox idCheckBox = new JCheckBox();
        idCheckBox.setAction(new AbstractAction("Show ids") {
            @Override
            public void actionPerformed(ActionEvent e) {
                controller.setIdVisible(idCheckBox.isSelected());
            }
        });
        menu.add(idCheckBox);
    }


    public SVGFrame(InputStream stream,
                    DependencyGraph g,
                    PrintingOptions opts,
                    PuckControl control) throws IOException {
        this.setLayout(new BorderLayout());
        SVGPanel panel = new SVGPanel(SVGController.documentFromStream(stream));

        SVGConsole console = new SVGConsole();

        controller = SVGController.apply(control, g, opts, panel.canvas, console);
        panel.setController(controller);
        controller.registerAsStackListeners(this);

        addVisibilityCheckBoxesToMenu();

        addUndoRedoButtonToMenu();

        menu.add(console.console);

        this.add(panel, BorderLayout.CENTER);
        this.add(menu, BorderLayout.SOUTH);


        this.setVisible(true);
        this.setMinimumSize(new Dimension(640, 480));
        this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    }
}
