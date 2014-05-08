package puck;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import AST.BodyDecl;

public class Utils {
	
	public static Map<String,Collection<BodyDecl>> initStringLiteralsMap(File file) {
		Map<String,Collection<BodyDecl>> smap = new HashMap<String,Collection<BodyDecl>>();
		try {
			BufferedReader readerHideStrings = new BufferedReader(new FileReader(file));
			
			
			String literalPatternBegining = Pattern.quote("string('");
			String literalPatternEnding =  Pattern.quote("')");
			
			Pattern pat = Pattern.compile( literalPatternBegining + "([^']*)" + literalPatternEnding);
			
			
			String line = readerHideStrings.readLine();
			while (line != null) {
				Matcher m = pat.matcher(line);
				while(m.find()){
					smap.put(m.group(1), new ArrayList<BodyDecl>());
				}
				line = readerHideStrings.readLine();
			}
			readerHideStrings.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return smap;
	}

	public static String cleanLiteral(String lit) {
		String cleanLit = lit;
		cleanLit = lit.replace("#SN#", "").replace("\'", "");
		return cleanLit;
	}
	
	public static void pipeStream(InputStream input, OutputStream output)throws IOException{
		byte buffer[] = new byte[1024];
		int numRead = 0;

		do	{
			numRead = input.read(buffer);
			output.write(buffer, 0, numRead);
		} while (input.available() > 0);

		output.flush();
	}

	public static void writeObject(String filename, Object o){
	   try{
		      ObjectOutput output = new ObjectOutputStream( new BufferedOutputStream( new FileOutputStream( filename ) ) );
		      try{
		        output.writeObject(o);
		      }
		      finally{
		        output.close();
		      }
		    }  
		    catch(IOException ex){
		    	ex.printStackTrace();
		    }
	}
	
	@SuppressWarnings("unchecked")
	public static <T> T readObject(String filename){
		try{
			ObjectInput input = new ObjectInputStream( new BufferedInputStream( new FileInputStream( filename ) ) );
			T t = null;
			try{
				t = (T)input.readObject();
			}
			finally{
				input.close();
			}
			return t;
			
		}
		catch(ClassNotFoundException ex){
	    	ex.printStackTrace();
		}
		catch(IOException ex){
	    	ex.printStackTrace();
		}
		return null;
	}
	
	/**
	 * assume the call is made from within a jar file,
	 * the filePath is from the root of the jar
	 */
	public static void unjarTextFile(File path)throws IOException{
			String filePath = path.getPath().replace('\\', '/');
			
			//System.out.println("searching "+filePath+" for extraction");
			InputStream in = Utils.class.getResourceAsStream("/" + filePath );
			if(in == null)
				throw new IOException("no /" + filePath+" found with the class loader");
			
			BufferedReader reader = new BufferedReader(new InputStreamReader(in));
			
			BufferedWriter writer = new BufferedWriter(new FileWriter(path));
			
			String line;
			while((line = reader.readLine())!=null){
				writer.write(line);
				writer.newLine();
			}
			writer.close();
			reader.close();
			in.close();
	}
	
	public static File canonicalFile(File f){
		try {
			return new File(f.getCanonicalPath());
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

}
