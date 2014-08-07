package puck

import java.io._

import puck.graph._
import puck.javaAG._
import puck.javaAG.nodeKind.JavaNodeKind
import puck.util.{SystemLogger, FileLogger, Logger}
import scala.sys.process.Process
import puck.graph.constraints.{DefaultDecisionMaker, DecisionMaker, ConstraintsParser}
import java.util.NoSuchElementException

class FilesHandler private (private [this] var srcDir : File,
                            private [this] var jarListFile0: File,
                            private [this] var apiNodesFile0 :File,
                            private [this] var decouple0 : File,
                            private [this] var graph0: File,
                            private [this] var logFile0 : File){


  private [this] var ag : JavaAccessGraph = _
  def graph = ag

  def srcDirectory = this.srcDir
  def srcDirectory_=(dir : File){ this.srcDir = dir.getCanonicalFile
    jarListFile0 = FilesHandler.defaultFile(srcDir, FilesHandler.Default.jarListFileName)
    apiNodesFile0 = FilesHandler.defaultFile(srcDir, FilesHandler.Default.apiNodesFileName)
    decouple0 = FilesHandler.defaultFile(srcDir, FilesHandler.Default.decoupleFileName)
    graph0 = FilesHandler.defaultFile(srcDir, FilesHandler.Default.graphFileName)
    logFile0 = FilesHandler.defaultFile(srcDir, FilesHandler.Default.logFileName)
  }

  def jarListFile = this.jarListFile0
  def jarListFile_=(f:File){ this.jarListFile0 = f.getCanonicalFile}

  def apiNodesFile = this.apiNodesFile0
  def apiNodesFile_=(f:File){this.apiNodesFile0= f.getCanonicalFile}

  private [this] var gdot : Option[File] = None
  def graphvizDot = this.gdot
  def graphvizDot_=(f: File){this.gdot = Some(f.getCanonicalFile)}
  def graphvizDot_=(sf: Option[File]){ sf match {
    case Some(f) => graphvizDot_=(f)
    case None => this.gdot = None
  }
  }

  def decouple = this.decouple0
  def decouple_=(f:File){this.decouple0 = f.getCanonicalFile}

  def graphStubFile = this.graph0
  def graphStubFile_=(f: File){this.graph0= f.getCanonicalFile}

  def loadGraph(ll : AST.LoadingListener) : AccessGraph[JavaNodeKind] = {
    FilesHandler.compile(FilesHandler.findAllJavaFiles(this.srcDirectory),
      puck.fileLines(jarListFile)) match {
      case None => throw new AGBuildingError("Compilation error, no AST generated")
      case Some(p) =>
        ag = p.buildAccessGraph(puck.initStringLiteralsMap(decouple), ll)
        fileLines(apiNodesFile).foreach {
          (l: String) =>
            val tab = l.split(" ")
            ag.addApiNode(p, tab(0), tab(1), tab(2))
        }
        ag

    }
  }

  def makeDot(printId : Boolean, printSignatures : Boolean, useOption : Option[AGEdge[JavaNodeKind]]){
    DotPrinter.print(new BufferedWriter(new FileWriter(graphStubFile.getCanonicalPath+".dot")),
      ag, JavaNode, printId, printSignatures, searchRoots = false, selectedUse = useOption)
  }

  def makeProlog(){
    PrologPrinter.print(new BufferedWriter(new FileWriter(graphStubFile.getCanonicalPath+".pl")), ag)
  }

  def solve (trace : Boolean = false,
             decisionMaker : DecisionMaker[JavaNodeKind] = new JavaDefaultDecisionMaker(graph)){

    graph.logger = new SystemLogger()
    graph.logger. verboseLevel = 1
    var inc = 0

    new JavaSolver(graph, decisionMaker).solve(
      if(trace) {() =>
        graph.logger.writeln("*****************************************************")
        graph.logger.writeln("*********** solve end of iteration %d *************".format(inc))
        graph.logger.writeln()
        makePng(printSignatures = true,
          soutput = Some(new FileOutputStream(
            new File(graphStubFile.getCanonicalPath + "_trace" + inc +".png"))))
        inc += 1
      }
      else
        () => ()

    )
  }


  val logger : Logger = new FileLogger(logFile0)
  logger.verboseLevel = 10

  def explore (trace : Boolean = false){

    val engine = new JavaConstraintSolvingSearchEngine(graph, logger,
      if(trace) { state =>
        state.isStep = true

        val f = new File("%s_traces%c%s".format(graphStubFile.getCanonicalPath,
          File.separatorChar, state.uuid(File.separator, "_", ".png")))

        logger.writeln("*****************************************************")
        logger.writeln("*********** solve end of iteration %d *****************".format(state.depth))
        logger.writeln("***********  %s ***************".format(f.getAbsolutePath))

        logger.writeln()

        f.getParentFile.mkdirs()
        makePng(soutput = Some(new FileOutputStream(f)))
      }
      else
        _ => ()
    )


    time(logger) {
      engine.explore()
    }

    var i = 0
    val d = new File("%s_results".format(graphStubFile.getCanonicalPath))
    d.mkdir()
    engine.finalStates.foreach { s =>
      s.internal.recording()
      makePng(soutput = Some(new FileOutputStream(
        new File("%s_results%c%04d.png".format(graphStubFile.getCanonicalPath,
          File.separatorChar, i)))))
      i += 1
    }

  }


  def dot2png(soutput : Option[OutputStream] = None) : Int = {
    val processBuilder = Process(List(
      graphvizDot match {
        case None => "dot" // relies on dot directory being in the PATH variable
        case Some(f) => f.getCanonicalPath
      } , "-Tpng", graphStubFile.getCanonicalPath+".dot"))

    soutput match {
      case None =>(processBuilder #> new File(graphStubFile.getCanonicalPath + ".png")).!
      case Some(output) => (processBuilder #> output).!
    }

  }

  def makePng(printId : Boolean = false,
              printSignatures : Boolean = false,
              soutput : Option[OutputStream] = None,
              selectedUse : Option[AGEdge[JavaNodeKind]] = None) : Int = {
    makeDot(printId, printSignatures, selectedUse)
    dot2png(soutput)
  }

  def parseConstraints() {
    val parser = new ConstraintsParser(graph)
    try {
      parser(new FileReader(decouple))
    } catch {
      case e : NoSuchElementException =>
        graph.discardConstraints()
        throw new AGError("parsing failed :" + e.getLocalizedMessage)

    }
  }

  def printCode(outDir : File = new File(srcDirectory + File.separator + "out")){
    val l : AST.List[AST.CompilationUnit] = graph.program.getCompilationUnits

    import scala.collection.JavaConversions.asScalaIterator
    asScalaIterator(l.iterator()).foreach{ cu =>

      if(cu.fromSource()) {
        val relativePath = cu.getPackageDecl.replace('.', File.separatorChar)

       /* println("my relativePath is " + relativePath)

        println("path name is " + cu.pathName())
        println("relative name is " + cu.relativeName())
        println("id is " + cu.getID)*/
        val f = new File(outDir + File.separator +
          relativePath + File.separator + cu.getID + ".java")

        f.getParent.mkdirs()
        val writer = new BufferedWriter(new FileWriter(f))
        writer.write(cu.toString())
        writer.close()
      }
    }

  }

}

object FilesHandler{
  object Default{
    final val decoupleFileName: String = "decouple.pl"
    final val graphFileName: String = "graph"
    final val jarListFileName: String = "jar.list"
    final val apiNodesFileName: String = "api_nodes"
    final val logFileName: String = "graph_solving.log"
  }

  private def defaultFile(dir:File, file: File) =
    new File(dir.getCanonicalFile + File.separator + file)

  def apply(srcDir: File)(jarListFile: File =
                          defaultFile(srcDir, Default.jarListFileName),
                          apiNodesFile :File =
                          defaultFile(srcDir, Default.apiNodesFileName),
                          decouple : File =
                          defaultFile(srcDir, Default.decoupleFileName),
                          graph : File =
                          defaultFile(srcDir, Default.graphFileName))
  = new FilesHandler(srcDir.getCanonicalFile, jarListFile, apiNodesFile, decouple, graph,
    defaultFile(srcDir, Default.logFileName))

  def apply() : FilesHandler= apply(new File("."))()

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

    if (f.process(arglist, br, jp)){
      Some(f.getProgram)}
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