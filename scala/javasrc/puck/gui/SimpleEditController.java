package puck.gui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import puck.package$;

public class SimpleEditController {
	
	private File editedFile;
	private JLabel title;
	private JTextArea jta;
	private boolean jtaDirty;
	private JPanel panel;
	
	private FileChangeListener changeFileListener;
	
	SimpleEditController(File f){
		
		
		editedFile = f;
		panel = new JPanel();
		panel.setPreferredSize(new Dimension(550, 150));

		GridBagLayout gridbag = new GridBagLayout();
		GridBagConstraints gbconstraints = new GridBagConstraints();
		panel.setLayout(gridbag);
		
		gbconstraints.fill = GridBagConstraints.BOTH;
		gbconstraints.weightx = 1.0;
		gbconstraints.weighty = 1.0;
		gbconstraints.gridwidth = GridBagConstraints.REMAINDER;
		
		title = new JLabel();
		gridbag.setConstraints(title, gbconstraints);
		panel.add(title);

		gbconstraints.weighty = 20.0;
		jta = new JTextArea();
		jta.addKeyListener(new KeyListener() {
			
			public void keyTyped(KeyEvent e) {
				jtaDirty = true;
			}
			
			public void keyReleased(KeyEvent e) {}
			
			public void keyPressed(KeyEvent e) {}
		});
		JScrollPane sp = new JScrollPane(jta);
		sp.setPreferredSize(new Dimension(550, 150));
		gridbag.setConstraints(sp, gbconstraints);
		panel.add(sp);
	
		gbconstraints.weighty = 1.0;
		gbconstraints.fill = GridBagConstraints.WEST;
		JPanel buttonPannel = new JPanel();
		panel.add(buttonPannel);
		gridbag.setConstraints(buttonPannel, gbconstraints);
		
		JButton selectButton = new JButton("Select");
		selectButton.setToolTipText("Select the coupling constraint file");
		buttonPannel.add(selectButton);
		selectButton.addActionListener(new ActionListener() {
			
			public void actionPerformed(ActionEvent ae) {
				

				JFileChooser jfc = new JFileChooser(editedFile.getParentFile());	
				jfc.setDialogTitle("Wich constraint file do you want to use ?");
				jfc.setFileSelectionMode(JFileChooser.FILES_ONLY); 
				jfc.showDialog(null, null);
				File f = jfc.getSelectedFile();
				if(f != null ){
					try {
						f = f.getCanonicalFile();
						setEditedFile(f);
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
		});
		
		JButton saveButton = new JButton("Save");
		buttonPannel.add(saveButton);
		saveButton.addActionListener(new ActionListener() {
			
			public void actionPerformed(ActionEvent e) {
				save();
			}
		});
		
		this.loadFile();
		
		

	}
	
	void loadFile(){
		title.setText(editedFile.getAbsolutePath());
		//A refaire proprement
		for(String str : scala.collection.JavaConversions.
                asJavaCollection(package$.MODULE$.fileLines(editedFile, true))){
			jta.append(str);
			jta.append(System.getProperty("line.separator"));
		}
		jtaDirty = false;
	}
	
	void setEditedFile(File newFile){
		if(this.editedFile.equals(newFile))
			return;

		if((this.editedFile.exists() && jtaDirty) || (!this.editedFile.exists() && jta.getText().length() > 0)){
			JOptionPane optionPane = new JOptionPane("Save "+editedFile.getAbsolutePath()+" ?", 
					JOptionPane.QUESTION_MESSAGE, 
					JOptionPane.YES_NO_OPTION);
	
			JDialog dialog = optionPane.createDialog(panel, "Save");
			dialog.setVisible(true);
			Object selectedValue = optionPane.getValue();
			if(selectedValue != null && selectedValue instanceof Integer){
				if(((Integer)selectedValue).intValue() == JOptionPane.OK_OPTION){
					this.save();
				}
			}
		}
		if(changeFileListener != null)
			changeFileListener.change(newFile);
		
		editedFile=newFile;
		jta.setText("");
		loadFile();
	}
	
	void setChangeFileListener(FileChangeListener changeFileListener){
		this.changeFileListener=changeFileListener;
	}
	
	void save(){
		try {
			if(!editedFile.exists()){
				try {
					editedFile.createNewFile();
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
			BufferedWriter writer = new BufferedWriter(new FileWriter(editedFile));
			writer.write(jta.getText());
			writer.close();
			jtaDirty = false;
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	JPanel getPanel(){
		return panel;
	}
}
