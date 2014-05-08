package puck

import java.io.File
import scala.List
import puck.graph.AGBuildingError

/**
 * Created by lorilan on 08/05/14.
 */
class FilesHandler private (private [this] var srcDir : File)
                    (var jlFile: File =
                      new File(srcDir.getCanonicalFile +
                                  File.separator + FilesHandler.defaultJarListFileName),
                     var anFile :File =
                     new File(srcDir.getCanonicalFile +
                       File.separator + FilesHandler.defaultApiNodesFileName)){

  private def this(srcDir: String) = this(new File(srcDir).getCanonicalFile)()

  def srcDirectory = this.srcDir
  def srcDirectory_=(dir : File){ this.srcDir = dir.getCanonicalFile }
  def srcDirectory_=(dir: String){srcDirectory_=(new File(dir))}

  def jarListFile = jlFile
  def jarListFile_=(f:File){ this.jlFile = f.getCanonicalFile}

  def apiNodesFile = anFile
  def apiNodesFile_=(f:File){this.anFile= f.getCanonicalFile}


  def loadGraph(ll : AST.LoadingListener){
    FilesHandler.compile (FilesHandler.findAllJavaFiles(this.srcDirectory),
      fileLines(jarListFile)) match {
      case None => throw new AGBuildingError("Compilation error, no AST generated")
      case Some(p) => p.buildAccessGraph(null, ll)
    }

    /*
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
     */

  }
}

object FilesHandler{
  final val defaultPlDecoupleFileName: String = "decouple.pl"
  final val defaultGraphFileName: String = "graph"
  final val defaultJarListFileName: String = "jar.list"
  final val defaultApiNodesFileName: String = "api_nodes"

  def apply(srcDir: File) = new FilesHandler(srcDir.getCanonicalFile) ()

  def findAllJavaFiles(f:File) : List[String] = findAllJavaFiles(List(), f)

  def findAllJavaFiles(res: List[String], f: File) : List[String] = {
    if (f.isDirectory)
      f.listFiles().foldLeft(res)(findAllJavaFiles)
    else {
      if (f.getName.endsWith(".java"))
        f.getPath :: res
      else
        res
    }
  }

  def compile(sources: List[String], jars: List[String]): Option[AST.Program] = {
    val arglist = createArglist(sources, jars, None)
    val f = new AST.Frontend {
      protected override def processWarnings(errors: java.util.Collection[_], unit: AST.CompilationUnit) {
      }
    }
    val br = new AST.BytecodeParser
    val jp = new AST.JavaParser {
      def parse(is: java.io.InputStream, fileName: String): AST.CompilationUnit = {
        (new parser.JavaParser).parse(is, fileName)
      }
    }
    if (f.process(arglist, br, jp))
      Some(f.getProgram)
    else
      None
  }

  private[puck] def createArglist(sources: List[String], jars: List[String],
                                  srcdirs:Option[List[String]]): Array[String] = {
    if (jars.length == 0) return sources.toArray

    val args : List[String] = "-classpath" :: jars.mkString("", File.pathSeparator, ".") :: (
      srcdirs match {
        case Some(dirs) =>"-sourcespath" :: dirs.mkString("", ":", ".") :: sources
        case None => sources
      })
    args.toArray
  }

}