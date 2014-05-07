package puck;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import AST.BodyDecl;
import AST.Program;
import puck.graph.AccessGraph;
import puck.gui.PuckGui;

public class Puck {

	public static List<String> evaluatorFiles;
	
	public static String evaluator_env_varname= "PUCK_EVALUATOR";
	
	public static final String evalRoot = "evaluator"; 
	public static final String defaultPlDecoupleFileName = "decouple.pl";
	public static final String defaultGraphFileName = "graph";
	public static final String defaultJarListFileName = "jar.list";
	public static final String defaultApiNodesFileName = "api_nodes";

	
	private static final String unZippedEvaluatorPath;
	
	static {
		evaluatorFiles= new ArrayList<String>();
		String tests = "tests";
		//	evaluatorFiles.add(root);
		unZippedEvaluatorPath=evalRoot + File.separator + "evaluator.pl";
		evaluatorFiles.add(unZippedEvaluatorPath);
		
		evaluatorFiles.add(evalRoot + File.separator + "constraints.pl");
		evaluatorFiles.add(evalRoot + File.separator + "graphInvariants.pl");
		evaluatorFiles.add(evalRoot + File.separator + "pl2dot.pl");
		//	evaluatorFiles.add(root + File.pathSeparator + tests);
		evaluatorFiles.add(evalRoot + File.separator + tests + File.separator + "wellFormednessTests.pl");
	}

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

	private AccessGraph graph;
	private ProgramHelper app;
	private PrologHandler plHandler;

	private boolean delegateFiltering;

	private boolean evalCreated; //flag to know if evaluator dir has to be deleted on exit

	public Puck(ProgramHelper app, PrologHandler plHandler){
		this.app = app;
		this.plHandler = plHandler;
		this.plHandler.setPl2dot(true);
		try {
			this.plHandler.setEvaluator(new File(evalRoot+File.separator+"evaluator.pl").getCanonicalFile());
		} catch (IOException e) {
			e.printStackTrace();
		}
		delegateFiltering = false;
		this.evaluatorInit();
	}


	private void evaluatorInit(){

		File evalRootFile = new File(evalRoot);
		File testsFile = new File(evalRoot + File.separator + "tests");

		Map<String,String> env = System.getenv();
		
		evalCreated = false;
		if(env.containsKey(evaluator_env_varname)){
			this.plHandler.setEvaluator(new File(env.get(evaluator_env_varname)));
		}
		else{
			
			evalCreated = ! evalRootFile.exists();
	
			testsFile.mkdirs();
			
			try{
				for(String s: evaluatorFiles){
					Utils.unjarTextFile(new File(s));
				}
				this.plHandler.setEvaluator(new File(unZippedEvaluatorPath));
			}catch(IOException e){
				//	System.err.println("!!! evaluator extraction fail !!!");
				//	e.printStackTrace();
				this.plHandler.setEvaluator(null);
			}
		}
		
	}

	public void cleanEvaluator(){
		File evalRootFile = new File(evalRoot);
		File testsFile = new File(evalRoot + File.separator + "tests");

		if(evalCreated){
			System.out.println("Deleting evaluator files ...");
			for(String s: evaluatorFiles){
				(new File(s)).delete();
			}
			testsFile.delete();
			evalRootFile.delete();
		}
	}

	public Puck(File f) throws IOException{
		this(new ProgramHelper(f), new PrologHandler(f));

	}

	public Puck() throws IOException{
		this((new File(".")).getCanonicalFile());
	}

	public AccessGraph getGraph() {
		return graph;
	}

	public PrologHandler getPrologHandler() {
		return plHandler;
	}

	public void setPrologHandler(PrologHandler plHandler) {
		this.plHandler = plHandler;
	}


	public ProgramHelper getApplication() {
		return app;
	}


	public void setDelegateFiltering(boolean b){
		this.delegateFiltering=b;
	}

	public AccessGraph loadGraph(AST.LoadingListener ll){
		Program p;

//		for(String n : app.getFilesToCompile())
//			System.out.println(n);
		
		try{
			p = CompileHelper.compile(app.getFilesToCompile());
		}
		catch(Error e){
			e.printStackTrace();
			throw e;
		}
		
		if( p == null ){
			throw new puck.graph.AGBuildingError("Compilation error, no AST generated");
		}
		

		File f = plHandler.getDecouple();

		Map<String,Collection<BodyDecl>> allStringUses = null;

		if(f.exists()){
			allStringUses = Utils.initStringLiteralsMap(f);
		}

		System.out.println("Begining AccessGraph generation ...");

		try{
			graph = p.buildAccessGraph(allStringUses, ll);
		}catch(Throwable e){
			e.printStackTrace();
		}
		
		System.out.println("AccessGraph construction finished");

		java.util.List<String> apiNodesFileLines = Utils.fileLines(app.getApiNodesFile(), false);
		if (apiNodesFileLines.size()>0) {
			for(String apiNode : apiNodesFileLines){
				String tab[] = apiNode.split(" ");
				graph.addApiNode(p, tab[0], tab[1], tab[2]);
			}

			graph.computeRoots();
		}
		return graph;
	}

    /*
	public void setGraphVisibility(Matcher packagesMatcher, Matcher classesMatcher){
		graph.setVisibility(false);
		graph.setVisiblePackages(packagesMatcher);
		graph.setVisibleClasses(classesMatcher);
	}
    */
	public void setRedUsesOnly(boolean ruo){
		if(ruo)
			plHandler.setGraphName("ru");
		else
			plHandler.setGraphName("graph");

		plHandler.setRu2dot(ruo);
		plHandler.setPl2dot(!ruo);

	}

}
