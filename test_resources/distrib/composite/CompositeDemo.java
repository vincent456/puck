package fileSystem;

import java.util.ArrayList;
import java.util.List;

class File {
   public File( String name ) {
      this.name = name;
   }
   public void display(String path){
      System.out.println(path+name);
   }
   public void ls() { display(""); }
   private String name;
}

class Directory {
   public Directory( String name ) {
      this.name = name;
   }
   public void add( File f ) {
      files.add( f );
   }
   public void add( Directory d ) {
      directories.add( d );
   }
   public void display(String path) {
      System.out.println(path + name);
      String npath = path + name +"/";
      for(File f: files)
         f.display(npath);
      for(Directory d: directories)
         d.display(npath); 
   }
   public void ls(){ display(""); }
   private String    name;
   private List<File> files = new ArrayList<File>();
   private List<Directory> directories = new ArrayList<Directory>();
}

public class CompositeDemo {

   public static void client(){
   	Directory d = new Directory("dir1");
      File  f = new File("a");
      d.add(f);
      f.ls();
      d.ls();
   }

   public static void main( java.lang.String[] args ) {
      client();
   }
}
