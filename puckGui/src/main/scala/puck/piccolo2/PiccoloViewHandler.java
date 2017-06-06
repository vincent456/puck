/**
 * Created by Vincent Hudry on 29/05/2017
 */

package puck.piccolo2;

import puck.control.PuckControl;
import puck.view.NodeKindIcons;
import puck.view.PuckMainPanel;
import puck.view.TreeViewHandler;
import puck.view.ViewHandler;
import scala.swing.Publisher;

public class PiccoloViewHandler extends ViewHandler {

    @Override
    public String toString(){
        return "Piccolo2 View";
    }

    @Override
    public Publisher installView(PuckMainPanel mainPanel, NodeKindIcons nodeKindIcons) {

                PuckControl control = mainPanel.control();

        return new TreeViewHandler(mainPanel,scala.swing.Component.wrap(new PiccoloCanvas(control,nodeKindIcons)));
    }
}