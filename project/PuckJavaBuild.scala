import sbt.Keys._
import sbt._

object PuckJavaBuild extends Build {

  //TODO make a beaver plugin that allow to pass arguments !
  def beaverTask(srcFile : File){
    import beaver.comp.ParserGenerator
    import beaver.comp.io.SrcReader
    import beaver.comp.run.Options
    import beaver.comp.util.Log
    import java.io.{PrintStream, ByteArrayOutputStream}

    try{
      val opts = new Options()
      opts.terminal_names = true //"-t"
      opts.use_switch = true //"-w"

      val srcReader = new SrcReader(srcFile)
      val log = new Log()
      ParserGenerator.compile(srcReader, opts, log)

      // val baos = new ByteArrayOutputStream()
      // val stream = new PrintStream(baos)
      log.report(srcFile.getName, srcReader)

      if(log.hasErrors){
        sys.error("Error while generating parser")
      }


    } catch {
      case e : Exception => sys.error(e.getMessage())
    }
  }


  def needUpdate(sources : Seq[File], target : File): Boolean = sources exists {
    f => !f.exists() || f.lastModified() > target.lastModified()
  }

  val jrrtHome = settingKey[File]("Jrrt build directory")

  val jrrtReadOnly = settingKey[File]("jrrt-read-only directory")

  val java14frontend = settingKey[File]("Java1.4Frontend directory")

  val java15frontend = settingKey[File]("Java1.5Frontend directory")

  val java16frontend = settingKey[File]("Java1.6Frontend directory")

  val controlFlowGraph = settingKey[File]("ControlFlowGraph directory")

  val java15comply = settingKey[Boolean]("will produce parser and scanner for java1.5 ")

  val jastaddSrcDir = settingKey[File]("Location of puck jrag files")

  val jastaddOutDir = settingKey[File]("Location of puck jrag files")

  /*
    Tasks
   */

  val ast = taskKey[Seq[File]]("use ast, jrag and jadd files to generates java")
  val astTask : Def.Setting[Task[Seq[File]]] = ast := {

    println("AST generation")
    val java14Files : PathFinder = java14frontend.value ** ("*.jrag" | "*.jadd" | "*.ast") filter
      {f : File => f.name match {
        case "BytecodeAttributes.jrag"
             | "BytecodeDescriptor.jrag"
             | "BytecodeReader.jrag" => false
        case _ => true
      }}

    val java15Files : PathFinder = java15frontend.value ** ( "*.ast" | "*.jrag" | "*.jadd")



    val cfgFiles : PathFinder = Seq( controlFlowGraph.value / "Nodes.ast",
      controlFlowGraph.value / "ControlFlowGraph.jrag",
      controlFlowGraph.value / "Sets.jrag",
      controlFlowGraph.value / "Exceptions.jrag",
      controlFlowGraph.value / "Alias.jrag",
      controlFlowGraph.value / "ReachingDefinitions.jrag",
      controlFlowGraph.value / "DotGeneration.jrag" )

    val jrrtDir = jrrtReadOnly.value
    val jrrtUtilFiles : PathFinder = (jrrtDir / "util") ** ("*.jrag" | "*.jadd" | "*.ast")


    val jrrtFiles2 : PathFinder =
      jrrtDir ** ("*.ast" | "*.jrag" | "*.jadd") --- jrrtUtilFiles ---
        Seq(jrrtDir / "tests" / "ProgramFactory.jrag",
          jrrtDir / "tests" / "RTXF.jrag",
          jrrtDir / "tests" / "Testing.jrag",
          jrrtDir / "undo" / "NoUndo.jrag",
          jrrtDir / "AccessibilityConstraints" / "SolverChoco.jrag",
          jrrtDir / "TypeConstraints" / "TypeConstraintSolving.jrag",
          jrrtDir / "TypeConstraints" / "CollectTypeConstraints.jrag")

    val puckFiles : PathFinder = jastaddSrcDir.value ** ("*.jrag" | "*.jadd")

    val jrrtFiles3 : PathFinder =
      Seq(jrrtDir / "TypeConstraints" / "TypeConstraintSolving.jrag",
        jrrtDir / "TypeConstraints" / "CollectTypeConstraints.jrag")

    val java16Files : PathFinder = java16frontend.value / "Override.jrag"


    val generated = jastaddOutDir.value / "AST" / "ASTNode.java"
    val mustUpdate =
      if(generated.exists()){
        val allFiles : PathFinder = java14Files +++ java15Files +++ java16Files +++
          cfgFiles +++ jrrtUtilFiles +++ jrrtFiles2 +++ puckFiles +++ jrrtFiles3
        needUpdate(allFiles.get, generated)
      }
      else
        true


    if(!mustUpdate)
      println("AST generation : no update needed")
    else {
      val orderedPaths = (java14Files.getPaths
        ++: java15Files.getPaths.sorted
        ++: java16Files.getPaths
        ++: cfgFiles.getPaths
        ++: jrrtUtilFiles.getPaths.sorted
        ++: jrrtFiles2.getPaths.sorted
        ++: puckFiles.getPaths
        ++: jrrtFiles3.getPaths)

      println("generating ast and weaving aspects")

      //val jastAddParserJar = baseDirectory.value / "project" / "lib" / "JastAddParser.jar"
      val jastAddJar = baseDirectory.value / "project" / "lib" / "jastadd2.jar"

      val retVal = Fork.java(new ForkOptions(bootJars = Seq(jastAddJar/*, jastAddParserJar*/) ), "jastadd.JastAdd" +: "--beaver"
        +: "--package=AST"
        +: ("--o=" + jastaddOutDir.value)
        +: "--rewrite"
        +: "--novisitcheck"
        +: "--noCacheCycle"
        +: "--noComponentCheck"
        +: "--refineLegacy"
        +: orderedPaths)

      if(retVal == 0) {
        println("ast creation success")
        IO.copyDirectory(java14frontend.value / "beaver",
          jastaddOutDir.value / "beaver",
          preserveLastModified = true)
      }
      else
        println("ast creation failure")

    }
    (PathFinder( jastaddOutDir.value / "beaver" ) * "*" +++ PathFinder(jastaddOutDir.value / "AST") * "*").get
  }

  val parser = taskKey[Seq[File]]("create java parser")
  val parserTask : Def.Setting[Task[Seq[File]]] = parser := {

    println("Parser generation")
    //helper function to avoid a call to beaver main method that uses System.exit

    (jastaddOutDir.value / "parser").mkdirs()

    val parserAll = jastaddOutDir.value / "parser" / "JavaParser.all"
    /* generate the parser phase 1, create a full .lalr specification from fragments */

    val java15Files = if(java15comply.value)
      (PathFinder(java15frontend.value) * "*.parser").get
    else
      Seq()

    val concatFiles = Seq(jrrtReadOnly.value / "util" / "preamble.parser",
      java14frontend.value / "parser" / "java14.parser",
      java14frontend.value / "parser" / "errorrecovery.parser" ) ++ java15Files

    val javaParserJava = jastaddOutDir.value / "parser" / "JavaParser.java"

    if(!needUpdate(concatFiles, parserAll) && javaParserJava.exists)
      println("Parser generation : no update needed")
    else {

      concat(parserAll, concatFiles)


      /* generate the parser phase 2, translating .lalr to .beaver */
      // val jastAddPaserJar = baseDirectory.value / "lib" / "JastAddParser.jar"
      // val beaverRtJar = baseDirectory.value / "lib" / "beaver-rt.jar"
      val javaParserBeaver = jastaddOutDir.value / "parser" / "JavaParser.beaver"
      // Process(Seq("java", "-cp", jastAddPaserJar.getPath + ":" + beaverRtJar.getPath,
      //   "Main", parserAll.getPath,  javaParserBeaver.getPath) ).!

      //Main class of JastAddParser.jar
      Main.main(Array(parserAll.getPath, javaParserBeaver.getPath))

      //val beaverAnt = baseDirectory.value / "lib" / "beaver-cc-0.9.11.jar"
      /* generate the parser phase 3, translating .beaver to .java */
      //Process(Seq("java", "-jar",  beaverAnt.getPath, "-t", "-w", javaParserBeaver.getPath )).!
      //beaver.comp.run.Make.main(Array("-t", "-w", javaParserBeaver.getPath))
      beaverTask(javaParserBeaver)

    }
    Seq(javaParserJava)
  }

  val scanner = taskKey[Seq[File]]("create java scanner")
  val scannerTask : Def.Setting[Task[Seq[File]]] = scanner := {
    println("Scanner generation")

    val scannerFlex = jastaddOutDir.value / "scanner" / "JavaScanner.flex"

    val files = if(java15comply.value)
      Seq(jrrtReadOnly.value / "util" / "preamble.flex",
        java14frontend.value / "scanner" / "macros.flex",
        java15frontend.value / "java15macros.flex",
        java14frontend.value / "scanner" / "rules_preamble.flex",

        jrrtReadOnly.value / "util" / "WhiteSpace.flex",
        jrrtReadOnly.value / "util" / "Comments.flex",
        java14frontend.value / "scanner" / "Keywords.flex",
        java15frontend.value / "java15keywords.flex",

        java14frontend.value / "scanner" / "Literals.flex",
        java15frontend.value / "java15literals.flex",

        java14frontend.value / "scanner" / "Separators.flex",
        java14frontend.value / "scanner" / "Operators.flex",
        java15frontend.value / "java15operators.flex",
        java15frontend.value / "java15identifiers.flex",

        java14frontend.value / "scanner" / "postamble.flex")
    else
      Seq(jrrtReadOnly.value / "util" / "preamble.flex",
        java14frontend.value / "scanner" / "macros.flex",
        java14frontend.value / "scanner" / "rules_preamble.flex",
        jrrtReadOnly.value / "util" / "WhiteSpace.flex",
        jrrtReadOnly.value / "util" / "Comments.flex",
        java14frontend.value / "scanner" / "Keywords.flex",
        java14frontend.value / "scanner" / "Literals.flex",
        java14frontend.value / "scanner" / "Separators.flex",
        java14frontend.value / "scanner" / "Operators.flex",
        java14frontend.value / "scanner" / "Identifiers.flex",
        java14frontend.value / "scanner" / "postamble.flex")

    val producedFiles =
      Seq(jastaddOutDir.value / "scanner" / "JavaScanner.java",
        jastaddOutDir.value / "scanner" / "Unicode.java")

    if( (producedFiles forall (_.exists)) && !needUpdate(files, scannerFlex))
      println("Scanner generation : no update needed")
    else {
      concat(scannerFlex, files)

      jflex.Main.generate(Array("--nobak",
        "--legacydot",
        "-d", (jastaddOutDir.value / "scanner").getPath,
        scannerFlex.getPath))

      IO.copyFile(java14frontend.value / "scanner" / "Unicode.java",
        jastaddOutDir.value / "scanner" / "Unicode.java",
        preserveLastModified = true)
    }
    producedFiles
  }

  def concat(target: File, files : Seq[File]): Unit ={
    IO.delete(target) // delete from previous runs if clean wasn't called
    IO.touch(target)
    for (f <- files) {
      val content = IO.read(f)
      IO.append(target, content)
    }
  }


  def puckJavaBuildSettings = Seq[Setting[_]](
    jrrtHome := baseDirectory.value / "jrrt",
    jrrtReadOnly := jrrtHome.value / "jrrt-read-only",
    java14frontend := jrrtHome.value / "Java1.4Frontend",
    java15frontend := jrrtHome.value / "Java1.5Frontend",
    java16frontend := jrrtHome.value / "Java1.6Frontend",
    controlFlowGraph := jrrtHome.value / "ControlFlowGraph",
    jastaddSrcDir := baseDirectory.value / "src" / "main" / "jrag",
    jastaddOutDir := sourceManaged.value / "main",
    java15comply := true,
    mainClass in Compile := Some("puck.Front"),
    (sourceGenerators in Compile) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue),
    (sourceGenerators in Test) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue),

    cleanFiles += jastaddOutDir.value,

    parallelExecution in test := false,  //cannot compile several program in parallel with jastadd

    //without this option, there is "cannot assign instance of scala.collection.immutable.List$SerializationProxy"
    // Cast exception raised in RecordingSerializationSpec ...
    fork := true,
    astTask, parserTask, scannerTask
  )

}