package puck;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.IOUtils;

public class PrologHandler {
	
	private File dotPath;
	public File getDotPath() {
		return dotPath;
	}

	public void setDotPath(File dotPath) {
		this.dotPath = dotPath;
	}

	public File getSwiplPath() {
		return swiplPath;
	}

	public void setSwiplPath(File swiplPath) {
		this.swiplPath = swiplPath;
	}

	private File swiplPath;
	
	private File genDir;
	
	private File plGraph;
	
	private String graphName;
	
	private File dotGraph;

	private File decouple;
	
	private File evaluator;
	
	private boolean pl2dot;
	private boolean ru2dot;
	private boolean ru2txt;

	public PrologHandler(File directory){
		this.setGenDir(directory);

		evaluator = null;
		
		this.pl2dot=false;
		this.ru2dot=false;
		this.ru2txt=false;
	}
	
	public PrologHandler(String fileName){
		this(new File(fileName));
	}
	
	public File getGenDir() {
		return genDir;
	}

	public void setGenDir(File genDir) {
		this.genDir = Utils.canonicalFile(genDir);
		
		String absPath = this.genDir.getAbsolutePath() + File.separator; 
		plGraph = new File(absPath + this.genDir.getName() + ".pl");
		graphName = FilesHandler.defaultGraphFileName();
		dotGraph = new File(absPath + graphName + ".dot");
		decouple = new File(absPath + FilesHandler.defaultDecoupleFileName());
	}
	
	public File getPlGraph() {
		return plGraph;
	}
	
	public File getPlGraphDirectory(){
		return plGraph.getParentFile();
	}
	
	public void setPlGraph(File plGraph) {
		this.plGraph = Utils.canonicalFile(plGraph);
	}

	public String getGraphName(){
		return graphName;
	}

	public void setGraphName(String name){
		graphName = name;
		setDotGraphFileName(name);
	}
	
	public File getDotGraph() {
		return dotGraph;
	}
	
	public void setDotGraph(File dotGraph) {
		this.dotGraph = Utils.canonicalFile(dotGraph);
	}

	private void setDotGraphFileName(String fileName) {
		this.dotGraph = new File(dotGraph.getParent()+File.separator+ fileName +".dot");
	}
	
	public File getDecouple() {
		return decouple;
	}

	public void setDecouple(File decouple) {
		this.decouple = Utils.canonicalFile(decouple);
	}

	public File getEvaluator() {
		return evaluator;
	}

	public void setEvaluator(File evaluator) {
		if(evaluator != null){
			this.evaluator = Utils.canonicalFile(evaluator);
		}
		else{
			this.evaluator=null;
		}
	}
	

	public void setPl2dot(boolean pl2dot) {
		this.pl2dot = pl2dot;
	}

    public boolean isPl2dot() {
        return pl2dot;
    }

    public boolean isRu2dot() {
		return ru2dot;
	}

	public void setRu2dot(boolean ru2dot) {
		this.ru2dot = ru2dot;
	}

	public boolean isRu2txt() {
		return ru2txt;
	}

	public void setRu2txt(boolean ru2txt) {
		this.ru2txt = ru2txt;
	}

	public String getGoal(){

		StringBuilder sb = new StringBuilder();
		sb.append("evaluate('");
		sb.append(getPlGraph().getAbsolutePath().replace('\\', '/')); //swipl use unix file separator even on windows
		sb.append( "', '");
		sb.append(getDecouple().getAbsolutePath().replace('\\', '/'));
		sb.append( "', '");
		sb.append(getDotGraph().getAbsolutePath().replace('\\', '/'));
		sb.append("').");
		
		return sb.toString();
	}
	
	
	
	public Process launchEvaluator() throws IOException{
		
		String swiplPath="swipl";
		
		if(getSwiplPath()!=null){
			try {
				swiplPath = getSwiplPath().getCanonicalPath();
			} catch (IOException e) {
				System.err.println("cannot resolve swipl path");
				return null;
			}
			
		}
		
		System.out.println("goal : "+getGoal());
		
		ProcessBuilder pb = new ProcessBuilder(swiplPath, 
								"-l", getEvaluator().getAbsolutePath(),
								"-g", getGoal(),
								"-t", "halt");

		System.out.println("launch evaluator process");
		
		Process pr = pb.start();
		IOUtils.copy(pr.getErrorStream(), System.out);
		IOUtils.copy(pr.getInputStream(), System.out);

		return pr;
	}
	
	public void dot2png(){
		
		String dotPath="dot";
		if(getDotPath()!=null){
			try {
				dotPath = getDotPath().getCanonicalPath();
			} catch (IOException e) {
				System.err.println("cannot resolve dot path");
				return;
			}
		}
		
		
		ProcessBuilder pb = new ProcessBuilder(dotPath, "-Tpng", getDotGraph().getAbsolutePath());
		pb.redirectOutput(new File(dotGraph.getParent()+File.separator+ getGraphName() +".png"));
	
		pb.redirectError();

		Process pr = null;
		try {
			pr = pb.start();
			IOUtils.copy(pr.getErrorStream(), System.out);
			IOUtils.copy(pr.getInputStream(), System.out);
			pr.waitFor();

		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		if(pr.exitValue() != 0){
			System.err.println("Dot to png conversion did not end properly");
		}
		
	}
	
/*	public void printGraph(AccessGraph graph){
		printGraph(graph, false);
	}

	public void printGraph(AccessGraph graph, boolean delegateFiltering){
		try {
			GraphPrinter printer;
			
			if(getPlGraph().exists())
				getPlGraph().delete();
			
			if(!getDecouple().exists())
				getDecouple().createNewFile();
			
			BufferedWriter writer = new BufferedWriter(new FileWriter(getPlGraph()));

			if(delegateFiltering){
				printer = new InvisiblePrinter(writer, graph);
			}
			else {
				printer = new FilteredPrinter(writer, graph);
			}
			
			printer.writePL();
			writer.flush();
			writer.close();
			
		} catch (IOException e) {
			e.printStackTrace();
		}

	}
	*/
}
