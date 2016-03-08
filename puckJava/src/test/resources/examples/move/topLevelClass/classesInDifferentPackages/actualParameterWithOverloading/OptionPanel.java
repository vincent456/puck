package p2;

import p1.TextTranslator;
import p1.ComboProperty;
import java.util.Vector;

public class OptionPanel implements TextTranslator{

	private Vector getControls() {

		Vector controls = new Vector();
        String[] ss = {"FreeMind.RESOURCE_LOOKANDFEEL"};

		controls.add(new ComboProperty(ss, this));
		controls.add(new ComboProperty(ss, controls));

        return controls;
    }

}
