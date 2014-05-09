package puck;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.OutputStream;


public class Utils {
	
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
}
