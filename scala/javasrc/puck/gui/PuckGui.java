package puck.gui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingWorker;

import puck.Puck;
import puck.graph.AccessGraph;

public class PuckGui extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JPanel buttonPanel;
	
	private int buttonPannelWidth = 120;
	private Dimension buttonDimension = new Dimension(buttonPannelWidth, 30);
	
	private JScrollPane packagePanel;
	private JTextArea jta;

	private Puck puck;

	private List<JComponent> buttonToShow;

	private PackagePanelController ppController;
	private SimpleEditController decoupleEditor;

	private JButton evaluatorStopButton;
	private ProcessKiller evaluatorInterrupter; 
	
	//private PackageOnlyCheckBox ponlyCheckBox;

	
	Puck getPuck(){
		return puck;
	}
	
	class PuckWorker extends SwingWorker<Void, Void>{

		private PuckGui puck;
		
		PuckWorker(PuckGui puck){
			this.puck= puck;
		}
		
		@Override
		protected Void doInBackground() throws Exception {
			puck.onRun();
			return null;
		}
		
	}

	private void onAppDirectoryChoice(){
		JFileChooser jfc = new JFileChooser(new File(puck.getApplication().getSrcDirectory().getAbsolutePath()));	
		jfc.setDialogTitle("What directory contains your Java application ?");
		jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY); 
		jfc.showDialog(null, null);
		File f = jfc.getSelectedFile();
		if(f != null ){
			try {
				f = f.getCanonicalFile();

				if(!f.getPath().equals(puck.getApplication().getSrcDirectory())){
					puck.getApplication().setSrcDirectory(f);
					puck.getPrologHandler().setGenDir(f);
					decoupleEditor.setEditedFile(puck.getPrologHandler().getDecouple());
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		System.out.println("Application directory : ");
		System.out.println(puck.getApplication().getSrcDirectory().getAbsolutePath());
	}

	private void onJarListFileChoice(){
		JFileChooser jfc = new JFileChooser(new File(puck.getApplication().getSrcDirectory().getAbsolutePath()));	
		jfc.setDialogTitle("Select the file containing a list of the jars needed to compile");
		jfc.setFileSelectionMode(JFileChooser.FILES_ONLY); 
		jfc.showDialog(null, null);
		File f = jfc.getSelectedFile();
		if(f != null ){
			try {
				f = f.getCanonicalFile();
				puck.getApplication().setJarListFile(f);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		System.out.println("Jars list file :\n" +puck.getApplication().getJarListFile().getAbsolutePath());
	}

	private void onEvaluatorChoice(){
		Frame maframe = new Frame();
		FileDialog choix1 = new FileDialog(maframe, "Which SWIProlog evaluator ?", FileDialog.LOAD);
		choix1.setVisible(true);

		if (choix1.getFile() != null) {
			puck.getPrologHandler().setEvaluator(new File(choix1.getDirectory()+choix1.getFile()));
			System.out.println("SWIProlog Evaluator ... OK.");
		}
		maframe.dispose();
	}

	private void onRun(){
		try{
			puck.writePL();
		}catch(Error e){
			System.out.println("writePL exception catched : "+ e.getMessage());
			e.printStackTrace();
			return;
		}
		
		System.out.println("Running the evaluator ... ");
		
		try {
			Process evalPr = puck.getPrologHandler().launchEvaluator();
			evaluatorInterrupter.setProcess(evalPr);
			evaluatorStopButton.setVisible(true);
			
			evalPr.waitFor();
			
			evaluatorInterrupter.setProcess(null);
			evaluatorStopButton.setVisible(false);
			
			if(evalPr.exitValue() != 0){
				System.out.println("Puck evaluator did not terminate normally");
			}
			
			
		} catch (IOException e1) {
			e1.printStackTrace();
		} catch (InterruptedException e) {
			System.err.println("Puck evaluator interrupted");
		}
		
		
		System.out.println("Begining dot to png conversion ... ");
		puck.getPrologHandler().dot2png();
		
		
		
		EventQueue.invokeLater(new Runnable(){
			public void run(){
				try{

					ImageFrame frame = new ImageFrame(puck.getPrologHandler().getPlGraph().getParentFile().getAbsolutePath()+File.separator+ 
							puck.getPrologHandler().getGraphName() +".png");
					frame.setVisible(true);
				}
				catch(ImageFrameError e){
					System.err.println(e.getMessage());
				}
			}
		});
	}
	


	void displayTree(AccessGraph g){

		if(ppController!=null)
			packagePanel.remove(ppController.getTreePanel());

		try{
			ppController = new PackagePanelController(g, puck.getApplication().getSrcDirectory().getName());
		}catch(Error e){
			return;
		}

		packagePanel.setViewportView(ppController.getTreePanel());
		for(JComponent c: buttonToShow){
			c.setVisible(true);
		}

	}

	

	private JButton makeButton(String name, String toolTip, ActionListener actl, boolean delayedDisplay){
		JButton b = new JButton(name);
		b.addActionListener(actl);
		//b.setAlignmentX(Component.LEFT_ALIGNMENT);
		b.setSize(buttonDimension);
		b.setPreferredSize(buttonDimension);
		b.setMinimumSize(buttonDimension);
		b.setMaximumSize(buttonDimension);
		buttonPanel.add(b);
		
		b.setToolTipText(toolTip);
		if(delayedDisplay){
			b.setVisible(false);
			buttonToShow.add(b);
		}
		return b;

	}

//	private abstract class JCheckBoxHandler implements ItemListener{
//		JCheckBox checkBox;
//		public JCheckBoxHandler(String text) {
//			checkBox = new JCheckBox(text);
//			buttonPanel.add(checkBox);
//			checkBox.setVisible(false);
//			buttonToShow.add(checkBox);
//			checkBox.addItemListener(this);
//		}
//	}

//	private class PackageOnlyCheckBox extends JCheckBoxHandler{
//		public PackageOnlyCheckBox() {
//			super("Packages Only (overrides view)");
//		}
//
//		public void itemStateChanged(ItemEvent e) {
//			if(checkBox.isSelected()){
//				ppController.setVisiblePackagesOnly();
//			}
//		}
//		public JCheckBox getCheckBox(){
//			return checkBox;
//		}
//
//	}
//
//	private class DelegateFilteringCheckBox extends JCheckBoxHandler{
//		public DelegateFilteringCheckBox() {
//			super("Delegate filtering to prolog");
//		}
//
//		public void itemStateChanged(ItemEvent e) {
//			puck.setDelegateFiltering(checkBox.isSelected());
//		}
//
//	}
//
//	private class RedUsesOnlyCheckBox extends JCheckBoxHandler{
//		public RedUsesOnlyCheckBox() {
//			super("Display \"constraint violating\" uses only");
//		}
//
//		public void itemStateChanged(ItemEvent e) {
//			puck.setRedUsesOnly(checkBox.isSelected());
//		}
//
//	}
	private void initButtons(){
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));

		
		makeButton("Settings", "To set swipl and dot path", new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent arg0) {
				EventQueue.invokeLater(new Runnable(){
					public void run(){
						try{
							SettingsFrame frame = new SettingsFrame(puck);
							frame.setVisible(true);
						}
						catch(ImageFrameError e){
							System.err.println(e.getMessage());
						}
					}
				});
				
			}
		}, false);
		
		makeButton("Application ?", 
				"Select the root directory containing the java (up to 1.5) source code you want to analyse",
				new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onAppDirectoryChoice();
			}
		},
		false);

		if(puck.getPrologHandler().getEvaluator() == null){
			makeButton("SWIProlog evaluator ?",
					"Select prolog file containing the coupling constraint evaluator",
					new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					onEvaluatorChoice();
				}
			},
			false);
		}


		makeButton("Jars list file", 
				"Select a file containing a list of the jar libraries required by the analysed program",
				new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onJarListFileChoice();
			}
		},
		false);

		final GraphLoader loader = new GraphLoader(this);
		
		makeButton("Load code",
				"Load the selected source code and build the access graph",
				new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loader.launch();
			}
		}, 
		false);
		
		
		buttonPanel.add(loader.getProgressBar());


		buttonToShow = new ArrayList<JComponent>();

		makeButton("View None",
				"Deselect all access graph node",
				new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				//ponlyCheckBox.getCheckBox().setSelected(false);
				ppController.setVisibilityAll(false);
			}
		}, true);

		makeButton("View All",
				"Select all access graph node",
				new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				//ponlyCheckBox.getCheckBox().setSelected(false);
				ppController.setVisibilityAll(true);
			}
		}, true);
		
		

//		ponlyCheckBox = new PackageOnlyCheckBox();
//		//new DelegateFilteringCheckBox();
//		new RedUsesOnlyCheckBox();

		makeButton("Run",
				"Launch the coupling constraint evaluation",
				new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new PuckWorker(PuckGui.this).execute();
				
			}
		}, true);
		
		evaluatorInterrupter = new ProcessKiller();
		evaluatorStopButton = makeButton("Stop Evaluator", "Stop the evaluator", evaluatorInterrupter, false);
		evaluatorStopButton.setVisible(false);

		

	}

//	private static void normalizeButtonsSize(List<JButton> btList){
//		int normalWidth = 0;
//		for(JButton b : btList){
//			if(b.getWidth()>normalWidth)
//				normalWidth = b.getWidth();
//		}
//		System.out.println("width = "+normalWidth);
//		for(JButton b : btList){
//			int h = b.getHeight();
//			Dimension d = new Dimension(normalWidth, h);
////			b.setSize(d);
////			b.setPreferredSize(d);
////			b.setMinimumSize(d);
//			b.setMaximumSize(d);
//		}
//	}


	public PuckGui(){
		super("Puck V1.2");
		try {
			puck = new Puck();
		} catch (IOException e) {
			e.printStackTrace();
		}

		this.setMinimumSize(new Dimension(550, 650));

		int dividersSize = 3;

		JSplitPane wrapper = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		this.add(wrapper);

		JSplitPane top = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		top.setResizeWeight(0.2);
		top.setDividerSize(dividersSize);

		JSplitPane textsPannel = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		textsPannel.setResizeWeight(0.4);
		textsPannel.setDividerSize(dividersSize);

		wrapper.setLeftComponent(top);
		wrapper.setRightComponent(textsPannel);
		wrapper.setResizeWeight(0.6); //
		wrapper.setDividerSize(dividersSize);


		buttonPanel = new JPanel(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
		buttonPanel.setPreferredSize(new Dimension(buttonPannelWidth, 300));
		top.setLeftComponent(buttonPanel);
		this.initButtons();
		
		packagePanel = new JScrollPane();
		packagePanel.setPreferredSize(new Dimension(350, 500));
		top.setRightComponent(packagePanel);

		jta = new JTextArea();
		JScrollPane sp = new JScrollPane(jta);
		sp.setPreferredSize(new Dimension(550, 150));
		textsPannel.setLeftComponent(sp);

		//redirect System.out to a JTextArea jta ~ Java Console
		PrintStream ps = new PrintStream(new puck.gui.TextOutputStream(jta));
		System.setOut(ps);
		System.setErr(ps);
		System.out.println("Application Directory : "+puck.getApplication().getSrcDirectory().getAbsolutePath());
		decoupleEditor = new SimpleEditController(puck.getPrologHandler().getDecouple());
		decoupleEditor.setChangeFileListener(new FileChangeListener() {
			public void change(File f) {
				puck.getPrologHandler().setDecouple(f);
			}
		});
		if(!puck.getApplication().getJarListFile().exists())
			System.out.println("No jars list file selected");
		else
			System.out.println("Jars list file : " +puck.getApplication().getJarListFile().getAbsolutePath());

		textsPannel.setRightComponent(decoupleEditor.getPanel());

		this.setVisible(true);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		this.addWindowListener(new OnExit());

	}

	class OnExit extends WindowAdapter{
		public void windowClosing(WindowEvent e){
			puck.cleanEvaluator();
		}
	}



}
