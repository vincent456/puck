package puck.gui;

import javax.swing.JProgressBar;
import javax.swing.SwingWorker;

import puck.graph.AccessGraph;
import puck.graph.AGBuildingError;

class GraphLoader implements AST.LoadingListener{

	private PuckGui gui;
	private JProgressBar progressBar;
	GraphLoader(PuckGui gui){
		this.gui = gui;
		progressBar = new JProgressBar(0, 100);
		progressBar.setValue(0);
		progressBar.setStringPainted(true);
		progressBar.setVisible(false);
		
	}
	
	JProgressBar getProgressBar(){
		return progressBar;
	}
	
	void load() throws Exception {
		progressBar.setVisible(true);
		progressBar.setValue(0);
		
		try{
			 AccessGraph ag = gui.getProgramHelper().loadGraph(this);
			 gui.displayTree(ag);
			 return;
		}
		catch(AGBuildingError e){
			System.err.println(e.getMessage());
			
		}
		return;
	}


	void done() {
		progressBar.setVisible(false);
		gui.repaint();
	}

	public void update(double loading) {
		progressBar.setValue( (int)(loading * 100) );
	}
	
	void launch(){
		new SwingWorker<Void, Void>(){

			@Override
			protected Void doInBackground() throws Exception {
				GraphLoader.this.load();
				return null;
			}
			
			@Override
			protected void done() {
				GraphLoader.this.done();
			}
		}.execute();
	}
}
