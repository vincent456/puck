package puck.gui;

import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.SwingWorker;

import puck.FilesHandler;
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

	private FilesHandler filesHandler;

	private List<JComponent> buttonToShow;

	private PackagePanelController ppController;
	private SimpleEditController decoupleEditor;

	private JButton evaluatorStopButton;
	private ProcessKiller evaluatorInterrupter; 
	
	//private PackageOnlyCheckBox ponlyCheckBox;


    FilesHandler getFilesHandler(){
		return filesHandler;
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





	private void onRun(){

		System.out.println("Running the evaluator ... ");
		
		try {
			Process evalPr = filesHandler.getPrologHandler().launchEvaluator();
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
		filesHandler.getPrologHandler().dot2png();
		
		
		
		EventQueue.invokeLater(new Runnable(){
			public void run(){
				try{

					ImageFrame frame = new ImageFrame(filesHandler.getPrologHandler().getPlGraph().getParentFile().getAbsolutePath()+File.separator+
							filesHandler.getPrologHandler().getGraphName() +".png");
					frame.setVisible(true);
				}
				catch(ImageFrameError e){
					System.err.println(e.getMessage());
				}
			}
		});
	}
	

	private void initButtons(){
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));

		
		final GraphLoader loader = new GraphLoader(this);
		

		
		
		buttonPanel.add(loader.getProgressBar());


		buttonToShow = new ArrayList<JComponent>();



		makeButton("Run",
				,
				new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new PuckWorker(PuckGui.this).execute();
				
			}
		}, true);
		
		evaluatorInterrupter = new ProcessKiller();
		evaluatorStopButton = makeButton("Stop Evaluator", "Stop the evaluator", evaluatorInterrupter, false);
		evaluatorStopButton.setVisible(false);

		

	}

	public PuckGui(){
		super("Puck V1.2");
		try {
			filesHandler = new FilesHandler();
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
		System.out.println("Application Directory : " + filesHandler.getSrcDirectory().getAbsolutePath());
		decoupleEditor = new SimpleEditController(filesHandler.getPrologHandler().getDecouple());
		decoupleEditor.setChangeFileListener(new FileChangeListener() {
			public void change(File f) {
				filesHandler.getPrologHandler().setDecouple(f);
			}
		});
		if(!filesHandler.getJarListFile().exists())
			System.out.println("No jars list file selected");
		else
			System.out.println("Jars list file : " + filesHandler.getJarListFile().getAbsolutePath());

		textsPannel.setRightComponent(decoupleEditor.getPanel());

		this.setVisible(true);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);


	}
}
