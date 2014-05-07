package puck.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class ProcessKiller implements ActionListener{

	private Process p;


	public void setProcess(Process p) {
		this.p = p;
		
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		if(p!= null){
			p.destroy();
		}
	}
	
}
