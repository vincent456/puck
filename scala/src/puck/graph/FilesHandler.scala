package puck.graph

import java.io._
import java.util.NoSuchElementException

import puck.graph.constraints._
import puck.search.SearchState
import puck.util.FileHelper.findAllFiles
import puck.util._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.sys.process.Process
import scala.util.{Try, Failure, Success}


sealed abstract class DotOutputFormat
case class Png() extends DotOutputFormat{
  override def toString = "png"
}
case class Svg() extends DotOutputFormat{
  override def toString = "svg"
}

abstract class FilesHandler[Kind <: NodeKind[Kind]](workindDirectory : File){

  private [this] var srcDir0 : Option[File] = None
  private [this] var outDir0 : Option[File] = None
  private [this] var jarListFile0 : Option[File] = None
  private [this] var apiNodesFile0 : Option[File] = None
  private [this] var decouple0 : Option[File] = None
  private [this] var logFile0 : Option[File] = None

  var graphStubFileName : String = FilesHandler.Default.graphFileName

  private [this] var logger0 : Logger[Int] = new DefaultSystemLogger()

  def logger : Logger[Int] = logger0
  def logger_=( l : Logger[Int]){logger0 = l}

  private [this] var ag : AccessGraph[Kind] = _
  def graph = ag
  protected def graph_=(g : AccessGraph[Kind]){ ag = g }


  def setCanonicalOptionFile(prev : Option[File], sf : Option[File]) = {
    sf match {
      case None => prev
      case Some(f) => val fc = f.getCanonicalFile
        if(fc.exists())
          Some(fc)
        else {
          logger.writeln("%s does not exists.".format(f))
          None
        }
    }
  }


  def setWorkingDirectory(dir : File){
    this.srcDir0 = setCanonicalOptionFile(this.srcDir0, Some(dir))
    this.srcDir0 match {
      case None => throw new AGError("Invalid working directory !!!")
      case Some(d) =>

        def defaultFile(fileName: String) =
          Some(new File( d + File.separator + fileName))

        import FilesHandler.Default

        val Some(od) = defaultFile(Default.outDirName)
        if(!od.exists()){
          od.mkdir()
        }
        outDir0 = Some(od)
        jarListFile0 = defaultFile(Default.jarListFileName)
        apiNodesFile0 = defaultFile(Default.apiNodesFileName)
        decouple0 = defaultFile(Default.decoupleFileName)
        logFile0 = defaultFile(Default.logFileName)
    }
  }

  setWorkingDirectory(workindDirectory)

  def srcDirectory = this.srcDir0
  def srcDirectory_=(sdir : Option[File]) {
    this.srcDir0 = setCanonicalOptionFile(this.srcDir0, sdir)
  }

  def outDirectory = this.outDir0
  def outDirectory_=(sdir : Option[File]){
    this.outDir0 = setCanonicalOptionFile(this.outDir0, sdir)
  }

  def jarListFile = this.jarListFile0
  def jarListFile_=(sf : Option[File]){
    this.jarListFile0 = setCanonicalOptionFile(this.jarListFile0, sf)
  }

  def apiNodesFile = this.apiNodesFile0
  def apiNodesFile_=(sf : Option[File]){
    this.apiNodesFile0 = setCanonicalOptionFile(this.apiNodesFile0, sf)
  }

  private [this] var gdot : Option[File] = None

  def graphvizDot = this.gdot
  def graphvizDot_=(sf: Option[File]){
    this.gdot = setCanonicalOptionFile(this.gdot, sf)
  }

  def decouple = this.decouple0
  def decouple_=(sf: Option[File]){
    this.decouple0 = setCanonicalOptionFile(this.decouple0, sf)
  }

  def graphFile(suffix : String) : File = outDirectory match {
    case None => throw new AGError("no output directory !!")
    case Some(d) => new File(d + File.separator + graphStubFileName + suffix)
  }



  def loadGraph(ll : AST.LoadingListener) : AccessGraph[Kind]

  val dotHelper : DotHelper[Kind]

  def makeDot(printId : Boolean,
              printSignatures : Boolean,
              useOption : Option[AGEdge[Kind]],
              writer : OutputStreamWriter = new FileWriter(graphFile(".dot"))){
    DotPrinter.print(new BufferedWriter(writer), ag, dotHelper, printId,
      printSignatures, searchRoots = false, selectedUse = useOption)
  }

  def makeProlog(){
    PrologPrinter.print(new BufferedWriter(new FileWriter(graphFile(".pl"))), ag)
  }

  def convertDot( sInput : Option[InputStream] = None,
                  sOutput : Option[OutputStream] = None,
                  outputFormat : DotOutputFormat) : Int = {

    val dot = graphvizDot match {
      case None => "dot" // relies on dot directory being in the PATH variable
      case Some(f) => f.getCanonicalPath
    }

    val processBuilder =
      sInput match {
        case None => Process(List(dot,
          "-T" + outputFormat, graphFile(".dot").toString))
        case Some(input) => Process(List(dot,
          "-T" + outputFormat)) #< input
      }

    sOutput match {
      case None =>(processBuilder #> graphFile( "." + outputFormat)).!
      case Some(output) =>(processBuilder #> output).!
    }
  }

  def makePng(printId : Boolean = false,
              printSignatures : Boolean = false,
              sOutput : Option[OutputStream] = None,
              outputFormat : DotOutputFormat = Png(),
              selectedUse : Option[AGEdge[Kind]] = None)
             (finish : Try[Int] => Unit = {case _ => ()}){

    //TODO fix bug when chaining the two function with a pipe
    // and calling it in "do everything"
    makeDot(printId, printSignatures, selectedUse)

    convertDot(sInput = None, sOutput, outputFormat)

/*
    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    println("launchig convert dot future")
    Future {
      convertDot(Some(pipedInput), sOutput, outputFormat)
    } onComplete finish
    println("post launching")


    makeDot(printId, printSignatures, selectedUse,
      writer = new OutputStreamWriter(pipedOutput))
    println("post make dot")
*/


  }

  def parseConstraints() {
    val parser = new ConstraintsParser(graph)
    try {
      decouple match{
        case None => throw new AGError("cannot parse : no decouple file given")
        case Some(f) => parser(new FileReader(f))
      }
    } catch {
      case e : NoSuchElementException =>
        graph.discardConstraints()
        throw new AGError("parsing failed :" + e.getLocalizedMessage)

    }
  }

  def decisionMaker() : DecisionMaker[Kind]

  def solver(dm : DecisionMaker[Kind], logger: Logger[Int]) : Solver[Kind]

  def solve (trace : Boolean = false,
             decisionMaker : DecisionMaker[Kind]){

    var inc = 0

    def printTrace(){
      makePng(printSignatures = true,
        sOutput = Some(new FileOutputStream(graphFile( "_trace" + inc +".png"))))()
      inc += 1
    }

    graph.transformations.startRegister()
    solver(decisionMaker, this.logger).solve(
      if(trace) {() =>
        this.logger.writeln("*****************************************************")
        this.logger.writeln("*********** solve end of iteration %d *************".format(inc))
        this.logger.writeln()
        printTrace()
      }
      else
        () => ()

    )
    graph.doMerges()
    if(trace) {
      this.logger.writeln("*****************************************************")
      this.logger.writeln("*****************   merge done   ********************")
      this.logger.writeln()
      printTrace()
    }
  }


  type PrinterType = SearchState[ConstraintSolvingChoices[Kind], Option[AGNode[Kind]]] => Unit

  def constraintSolvingSearchEngine(graph : AccessGraph[Kind],
                                    searchEnginelogger: Logger[Int],
                                    solverLogger: Logger[Int],
                                    printer : PrinterType) : ConstraintSolvingSearchEngine[Kind]


  def explore (trace : Boolean = false){

    val searchEngineLogger = new DefaultFileLogger(logFile0.get)
    searchEngineLogger.verboseLevel = 10

    val engine = constraintSolvingSearchEngine(graph, searchEngineLogger, this.logger,
      if(trace) { state =>
        state.isStep = true

        val f = graphFile("_traces%c%s".format(
          File.separatorChar, state.uuid(File.separator, "_", ".png")))

        this.logger.writeln("*****************************************************")
        this.logger.writeln("*********** solve end of iteration %d *****************".format(state.depth))
        this.logger.writeln("***********  %s ***************".format(f.getAbsolutePath))

        this.logger.writeln()

        f.getParentFile.mkdirs()
        makePng(sOutput = Some(new FileOutputStream(f)))()
      }
      else
        _ => ()
    )


    puck.util.Time.time(logger) {
      engine.explore()
    }

    var i = 0
    val d = graphFile("_results")
    d.mkdir()
    engine.finalStates.foreach { s =>
      s.internal.recording()
      makePng(sOutput = Some(new FileOutputStream(
        graphFile("_results%c%04d.png".format(File.separatorChar, i)))))()
      i += 1
    }

  }

  def printCode() : Unit

  //TODO change to File
  //private [this] var editor0 : File = _
  private [this] var editor0 : String = "sublime_text"

  def editor = editor0
  def editor_=(editor : File){ editor0 = editor.getCanonicalPath }

  //TODO ? change to List[String] ?
  val srcSuffix : String

  private def openList(files : List[String]){ Process(editor  :: files ).! }

  def openSources() = openList(findAllFiles(srcDirectory.get, srcSuffix,
    outDirectory.get.getName))
  def openProduction() = openList(findAllFiles(outDirectory.get, srcSuffix,
    outDirectory.get.getName))

}

object FilesHandler{
  object Default{
    final val srcDirName : String = "src"
    final val outDirName : String = "out"
    final val decoupleFileName: String = "decouple.pl"
    final val graphFileName: String = "graph"
    final val jarListFileName: String = "jar.list"
    final val apiNodesFileName: String = "api_nodes"
    final val logFileName: String = "graph_solving.log"
  }
}