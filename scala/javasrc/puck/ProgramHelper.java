package puck;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class ProgramHelper {
	
	//srcs
	private File srcDirectory;
	private File jarListFile;
	private List<String> srcFiles; //relativePath with srcDirectory as Root
	
	
	private String apiNodesFileName;
	

	public ProgramHelper(File f) throws IOException{
		this.srcDirectory = f;
		
		try{
			this.srcDirectory = new File(this.srcDirectory.getCanonicalPath());
		}
		catch(IOException e){
			e.printStackTrace();
			throw e;
		}
	
		this.jarListFile = new File(srcDirectory.getAbsoluteFile() + File.separator + Puck.defaultJarListFileName);
		
		srcFiles = new ArrayList<String>();
		
		
		this.apiNodesFileName = Puck.defaultApiNodesFileName;

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
			sources.addAll(Utils.findAllJavaFiles(this.getSrcDirectory()));
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
	
	
	public void setApiNodesFileName(String apiNodesFileName){
		this.apiNodesFileName = apiNodesFileName;
	}
	
	public File getApiNodesFile(){
		return getFile(apiNodesFileName);
	}

}
