package puck.gui.imageDisplay;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;
import java.io.InputStream;

/**
 * Created by lorilan on 3/13/15.
 */
public class SVGFrame extends JFrame{
    public SVGFrame(InputStream stream) throws IOException {
        SVGPanel panel = SVGPanel.fromStream(stream);
        this.add(panel);
        this.setVisible(true);
        this.setMinimumSize(new Dimension(640,480));
    }
}
