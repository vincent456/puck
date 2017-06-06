package puck.piccolo2.menu.Actions;

import org.piccolo2d.event.PBasicInputEventHandler;
import org.piccolo2d.event.PInputEvent;
import org.piccolo2d.event.PInputEventFilter;

import java.awt.event.InputEvent;

/**
 * Created by Vincent Hudry on 01/06/2017.
 */
public abstract class MenuItemEventHandler extends PBasicInputEventHandler {

    public MenuItemEventHandler(){
        setEventFilter(new PInputEventFilter(InputEvent.BUTTON1_MASK));
    }

    @Override
    public abstract void mouseClicked(PInputEvent e);
}
