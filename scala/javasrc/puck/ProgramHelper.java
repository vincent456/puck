package puck;

import AST.BodyDecl;
import AST.Program;
import puck.graph.AccessGraph;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public class ProgramHelper {
	
	//srcs
	private File srcDirectory;
	private File jarListFile;
	private List<String> srcFiles; //relativePath with srcDirectory as Root
	
	
	private String apiNodesFileName;

    public ProgramHelper() throws IOException{
        this((new File(".")).getCanonicalFile());
    }


	public ProgramHelper(File f) throws IOException{
		this.srcDirectory = f;

        this.plHandler = new PrologHandler(f);
        this.plHandler.setPl2dot(true);
        delegateFiltering = false;

        try{
			this.srcDirectory = new File(this.srcDirectory.getCanonicalPath());
		}
		catch(IOException e){
			e.printStackTrace();
			throw e;
		}
	
		this.jarListFile = new File(srcDirectory.getAbsoluteFile() + File.separator + Front.defaultJarListFileName);
		
		srcFiles = new ArrayList<String>();
		
		
		this.apiNodesFileName = Front.defaultApiNodesFileName;

	}
	
	public ProgramHelper(String directory) throws IOException{
		this(new File(directory));
	}
	
	public void setSrcDirectory(File directory){
		try {
			File dir = new File(directory.getCanonicalPath());
			this.srcDirectory = dir;
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void setSrcDirectory(String directory){
		setSrcDirectory(new File(directory));
	}
	
	public File getSrcDirectory(){
		return this.srcDirectory;
	}
	
	public void addSrcFile(String relativeFilePath){
		srcFiles.add(relativeFilePath);
	}
	
	private static File getFile(File directory, String filename){
		return new File(directory.getPath()+File.separator+filename);
	}
	
	private File getFile(String filename){
		return getFile(this.srcDirectory, filename);
	}
	
	
	public void setJarListFile(File jarListFile){
		this.jarListFile = jarListFile;
	}
	
	public File getJarListFile(){
		return this.jarListFile;
	}
	
	public class SrcError extends Error{
		private static final long serialVersionUID = 1L;

		public SrcError(String msg){super(msg);}
	}
	
	public String[] getFilesToCompile(){
		List<String> sources= new ArrayList<String>();
		if(srcFiles.size()==0){
			sources.addAll(CompileHelper.findAllJavaFiles(this.getSrcDirectory()));
		}
		else{
			String dir = this.getSrcDirectory().getAbsolutePath();
			for(String f: srcFiles){
				sources.add(dir + File.separator + f);
			}
		}
		
		if(sources.size()==0)
			throw new SrcError("No sources");
		
		List<String> jarFiles = Utils.fileLines(this.jarListFile, false);

		System.out.println("list of jars :");
		for(String s :jarFiles)
			System.out.println(s);
		System.out.println("--- end of list ---");
		
		sources.addAll(jarFiles);
		
//		sources.addAll(Utils.fileLines(this.jarListFile, false));
		
	
		return sources.toArray(new String[]{});
	}



    private AccessGraph graph;
    private PrologHandler plHandler;

    private boolean delegateFiltering;

    private boolean evalCreated; //flag to know if evaluator dir has to be deleted on exit

    public AccessGraph getGraph() {
        return graph;
    }

    public PrologHandler getPrologHandler() {
        return plHandler;
    }

    public void setPrologHandler(PrologHandler plHandler) {
        this.plHandler = plHandler;
    }


    public void setApiNodesFileName(String apiNodesFileName){
		this.apiNodesFileName = apiNodesFileName;
	}
	
	public File getApiNodesFile(){
		return getFile(apiNodesFileName);
	}


    public void setDelegateFiltering(boolean b){
        this.delegateFiltering=b;
    }

    public AccessGraph loadGraph(AST.LoadingListener ll){
        Program p;

//		for(String n : app.getFilesToCompile())
//			System.out.println(n);

        try{
            p = CompileHelper.compile(getFilesToCompile());
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

        java.util.List<String> apiNodesFileLines = Utils.fileLines(getApiNodesFile(), false);
        if (apiNodesFileLines.size()>0) {
            for(String apiNode : apiNodesFileLines){
                String tab[] = apiNode.split(" ");
                graph.addApiNode(p, tab[0], tab[1], tab[2]);
            }

            graph.attachNodesWithoutContainer();
        }
        return graph;
    }

    /*
	public void setGraphVisibility(Matcher packagesMatcher, Matcher classesMatcher){
		graph.setVisibility(false);
		graph.setVisiblePackages(packagesMatcher);
		graph.setVisibleClasses(classesMatcher);
	}
	public void setRedUsesOnly(boolean ruo){
		if(ruo)
			plHandler.setGraphName("ru");
		else
			plHandler.setGraphName("graph");

		plHandler.setRu2dot(ruo);
		plHandler.setPl2dot(!ruo);

	}
    */

}
