package puck.gui.svg;

import puck.graph.DependencyGraph;
import puck.graph.io.PrintingOptions;
import puck.gui.PuckControl;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

public class SVGFrame extends JFrame implements StackListener {

    static public class SVGConsole {
        int lines = 10;
        int charPerLine = 50;

        public SVGConsole(){
            panel = new JPanel();
            panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
            panel.add(selection);
            panel.add(textArea);
            textArea.setEditable(false);
        }

        private JPanel panel;
        private JLabel selection = new JLabel();
        private JTextArea textArea = new JTextArea(lines, charPerLine);

        void displaySelection(String node){
            if(node.length()>0)
                selection.setText("Selection : " + node);
            else
                selection.setText("No selection");
        }
//        public void setText(String txt) {
//            textArea.setText(txt);
//        }
        public void appendText(String txt) {
            textArea.append(txt+"\n");
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

    private File chooseFile(){
        JFileChooser chooser = new JFileChooser();
                /*FileNameExtensionFilter filter = new FileNameExtensionFilter(
                        "JPG & GIF Images", "jpg", "gif");
                chooser.setFileFilter(filter);*/
        int returnVal = chooser.showOpenDialog(SVGFrame.this);
        if(returnVal == JFileChooser.APPROVE_OPTION) {
           return chooser.getSelectedFile();
        }
        return null;
    }

    private void addLoadSaveButton(){
        menu.add(new JButton(new AbstractAction("Save"){
            @Override
            public void actionPerformed(ActionEvent e) {
                File f= chooseFile();
                if(f == null) {
                    console.appendText("no file selected");
                }
                controller.saveRecordOnFile(f);
            }
        }));

        menu.add(new JButton(new AbstractAction("Load"){
            @Override
            public void actionPerformed(ActionEvent e) {
                File f= chooseFile();
                if(f == null) {
                    console.appendText("no file selected");
                }
                controller.loadRecord(f);
            }
        }));

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

    private final SVGConsole console;

    public SVGFrame(InputStream stream,
                    DependencyGraph g,
                    PrintingOptions opts,
                    PuckControl control) throws IOException {
        this.setLayout(new BorderLayout());
        SVGPanel panel = new SVGPanel(SVGController.documentFromStream(stream));

        console = new SVGConsole();

        controller = SVGController.apply(control, g, opts, panel.canvas, console);
        panel.setController(controller);
        controller.registerAsStackListeners(this);

        addVisibilityCheckBoxesToMenu();

        addUndoRedoButtonToMenu();

        addLoadSaveButton();

        menu.add(console.panel);

        menu.add(new JButton(new AbstractAction("Apply") {
            @Override
            public void actionPerformed(ActionEvent e) {
                controller.applyOnCode();
            }
        }));

        this.add(panel, BorderLayout.CENTER);
        this.add(menu, BorderLayout.SOUTH);


        this.setVisible(true);
        this.setMinimumSize(new Dimension(640, 480));
        this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    }
}
