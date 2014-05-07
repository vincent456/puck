package puck;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import puck.gui.PuckGui;

public class Front {

	public static final String defaultPlDecoupleFileName = "decouple.pl";
	public static final String defaultGraphFileName = "graph";
	public static final String defaultJarListFileName = "jar.list";
	public static final String defaultApiNodesFileName = "api_nodes";

	
	public static void main(String[] args) {
			try {
				UIManager.setLookAndFeel(
				        UIManager.getSystemLookAndFeelClassName());
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
			} catch (InstantiationException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (UnsupportedLookAndFeelException e) {
				e.printStackTrace();
			}
			
			new PuckGui();
	}

}
