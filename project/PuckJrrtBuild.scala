import sbt.Keys._
import sbt._

object PuckJrrtBuild {

  //TODO make a beaver plugin that allow to pass arguments !
  def beaverTask(srcFile : File){
    import beaver.comp.ParserGenerator
    import beaver.comp.io.SrcReader
    import beaver.comp.run.Options
    import beaver.comp.util.Log

    try{
      val opts = new Options()
      opts.terminal_names = true //"-t"
      opts.use_switch = true //"-w"

      val srcReader = new SrcReader(srcFile)
      val log = new Log()
      ParserGenerator.compile(srcReader, opts, log)

      log.report(srcFile.getName, srcReader)

      if(log.hasErrors){
        sys.error("Error while generating parser")
      }
    } catch {
      case e : Exception => sys.error(e.getMessage)
    }
  }


  def needUpdate(sources : Seq[File], target : File): Boolean = sources exists {
    f => !f.exists() || f.lastModified() > target.lastModified()
  }

  val jragSrcRoot = settingKey[File]("jrag root directory")

  val extendjRoot = settingKey[File]("extendj root directory")

  val java4 = settingKey[File]("Java 4 directory")

  val java5 = settingKey[File]("Java 5 directory")

  val java6 = settingKey[File]("Java 6 directory")

  val java7 = settingKey[File]("Java 7 directory")

  val java8 = settingKey[File]("Java 8 directory")


  val jrrtHome = settingKey[File]("Jrrt build directory")

  val controlFlowGraph = settingKey[File]("ControlFlowGraph directory")

  val puckJragJaddSrc = settingKey[File]("Location of puck jrag files")

  val jastaddOutDir = settingKey[File]("Location of puck jrag files")

  /*
    Tasks
   */

  val ast = taskKey[Seq[File]]("use ast, jrag and jadd files to generates java")
  val astTask : Def.Setting[Task[Seq[File]]] = ast := {

    println("AST generation")

    val j4frontend = java4.value / "frontend"
    val j5frontend = java5.value / "frontend"
    val j6frontend = java6.value / "frontend"
    val j7frontend = java7.value / "frontend"
    val j8frontend = java8.value / "frontend"

    def grammar(dir : File) : PathFinder = dir / "grammar" ** "*.ast"


    val java4grammar = grammar(java4.value) filter {
        _.name match {
          case "CatchClause.ast"
            | "Literals.ast" => false
          case _ => true
        }
      }

    val j8variable : PathFinder = j8frontend / "Variable.jadd"

    val j7constant : PathFinder = j7frontend / "Constant.jadd"

    val java4others = j4frontend ** ("*.jrag" | "*.jadd" ) filter {
      _.name match {
        case "BytecodeAttributes.jrag"
        | "BytecodeDescriptor.jrag"
        | "BytecodeReader.jrag"
        | "Constant.jadd"
        | "Literals.jrag"
        | "Variable.jadd" => false
        case _ => true
      }}

    val java5grammar = grammar(java5.value)

    val java5others = j5frontend ** ("*.jrag" | "*.jadd" ) filter {
      _.name != "BytecodeReader.jrag"
      }

    val java6files = j6frontend ** ("*.jrag" | "*.jadd" )
    val java8grammar = grammar(java8.value)

    val java7grammar = grammar(java7.value) filter {
      _.name != "BasicTWR.ast"
    }
    val java7others = j7frontend ** ("*.jrag" | "*.jadd" ) filter {
      _.name match {
        case "Constant.jadd"
             | "Variable.jadd" => false
        case _ => true
      }}
    val java8others = j8frontend  ** ("*.jrag" | "*.jadd" ) filter {
      _.name match {
        case "FrontendMain.jrag"
             | "Variable.jadd" => false
        case _ => true
      }}


    val cfgFiles =  controlFlowGraph.value * "*"
    val jrrtUtil =  jrrtHome.value / "util" ** ("*.jrag" | "*.jadd" | "*.ast" )
    val jrrtOthers = jrrtHome.value ** ("*.jrag" | "*.jadd" | "*.ast" ) --- cfgFiles --- jrrtUtil

    val puckFiles : PathFinder = puckJragJaddSrc.value ** ("*.jrag" | "*.jadd" | "*.ast")


    val generated = jastaddOutDir.value / "org" / "extendj"/ "ast" / "ASTNode.java"
    val mustUpdate =
      (!generated.exists()) || {
        val allFiles : PathFinder = java4grammar +++ j8variable +++ j7constant +++
          java4others +++ java5grammar +++ java5others +++ java6files +++ java8grammar +++
          java7grammar +++ java7others +++ java8others +++ cfgFiles +++ jrrtUtil +++ jrrtOthers +++ puckFiles

        needUpdate(allFiles.get, generated)
      }


    if(!mustUpdate) println("AST generation : no update needed")
    else {

      val orderedPaths =
        ( java4grammar.getPaths
        ++: j8variable.getPaths
        ++: j7constant.getPaths
        ++: java4others.getPaths.sorted
        ++: java5grammar.getPaths
        ++: java5others.getPaths.sorted
        ++: java6files.getPaths
        ++: java8grammar.getPaths
        ++: java7grammar.getPaths
        ++: java7others.getPaths.sorted
        ++: java8others.getPaths
        ++: cfgFiles.getPaths.sorted
        ++: jrrtUtil.getPaths.sorted
        ++: jrrtOthers.getPaths
        ++: puckFiles.getPaths.sorted)

      println("generating ast and weaving aspects")

      val jastAddJar = baseDirectory.value / "project" / "lib" / "jastadd2.jar"

      val retVal = Fork.java(new ForkOptions(bootJars = Seq(jastAddJar/*, jastAddParserJar*/) ),
        "jastadd.JastAdd"
          +: "--package=org.extendj.ast"
          +: "--rewrite=true"
          +: "--beaver"
          +: "--visitCheck=false"
          +: "--cacheCycle=false"
          +: ("--o=" + jastaddOutDir.value)
          +: "--defaultMap=new org.jastadd.util.RobustMap(new java.util.HashMap())"
          //+: "--noComponentCheck"
          //+: "--refineLegacy"
          +: orderedPaths)

      if(retVal != 0)
        sys.error("ast creation failure")



        IO.copyDirectory(extendjRoot.value / "src" / "frontend",
          jastaddOutDir.value,
          preserveLastModified = true)


    }

    (jastaddOutDir.value / "org" ** "*.java" ---
      genRelativePath(jastaddOutDir.value )  / "scanner" / "JavaScanner.java" ---
      genRelativePath(jastaddOutDir.value )  / "parser" / "JavaParser.java" +++
      jastaddOutDir.value / "beaver" * "*" ).get
  }

  val parser = taskKey[Seq[File]]("create java parser")
  val parserTask : Def.Setting[Task[Seq[File]]] = parser := {

    println("Parser generation")
    //helper function to avoid a call to beaver main method that uses System.exit

    val parserDir = genRelativePath(jastaddOutDir.value) / "parser"

    parserDir.mkdirs()

    val parserAll = parserDir / "JavaParser.all"
    val javaParserJava = parserDir / "JavaParser.java"

    /* generate the parser phase 1, create a full .lalr specification from fragments */
    val j4dir = java4.value / "parser"

    val java4files = Seq(
      j4dir / "Header.parser",
      j4dir / "Preamble.parser",
      j4dir / "Java1.4.parser",
      j4dir / "ErrorRecovery.parser")

    import scala.language.postfixOps
    val java5files = java5.value / "parser" ** "*.parser" get
    val java7files = java7.value / "parser" ** "*.parser" get
    val java8files = java8.value / "parser" ** "*.parser" get



    val concatFiles = java4files ++ java5files ++ java7files ++ java8files

    if(!needUpdate(concatFiles, parserAll) && javaParserJava.exists)
      println("Parser generation : no update needed")
    else {
      concat(parserAll, concatFiles)

      /* generate the parser phase 2, translating .lalr to .beaver */
      val javaParserBeaver = parserDir / "JavaParser.beaver"

      //Main class of JastAddParser.jar
      Main.main(Array(parserAll.getPath, javaParserBeaver.getPath))

      beaverTask(javaParserBeaver)

    }
    Seq(javaParserJava)
  }

  val scanner = taskKey[Seq[File]]("create java scanner")
  val scannerTask : Def.Setting[Task[Seq[File]]] = scanner := {
    println("Scanner generation")
    //val tmpDir = IO.createTemporaryDirectory
    val scannerFlex = jastaddOutDir.value / "scanner" / "JavaScanner.flex"

    val j4dir = java4.value / "scanner"
    val j5dir = java5.value / "scanner"
    val j7dir = java7.value / "scanner"
    val j8dir = java8.value / "scanner"

    val filesToConcat = Seq(
      j4dir / "Header.flex",
      j8dir / "Preamble.flex",
      j7dir / "Macros.flex",
      j4dir / "RulesPreamble.flex",
      j4dir / "WhiteSpace.flex",
      j4dir / "Comments.flex",
      j4dir / "Keywords.flex",
      j5dir / "Keywords.flex",
      j7dir / "Literals.flex",
      j4dir / "Separators.flex",
      j4dir / "Operators.flex",
      j8dir / "Separators.flex",
      j8dir / "Operators.flex",
      j5dir / "Operators.flex",
      j5dir / "Identifiers.flex",
      j4dir / "Postamble.flex")


    val scannerDir = genRelativePath(jastaddOutDir.value) / "scanner"
    val scannerJavaFile = scannerDir / "JavaScanner.java"

    if( scannerJavaFile.exists && !needUpdate(filesToConcat, scannerFlex))
      println("Scanner generation : no update needed")
    else {
      concat(scannerFlex, filesToConcat)

      jflex.Main.generate(Array("--nobak",
        "--legacydot",
        "-d", scannerDir.getPath,
        scannerFlex.getPath))


    }
    Seq(scannerJavaFile)
  }


  def genRelativePath(root : File) : File = root / "org" / "extendj"


  def concat(target: File, files : Seq[File]): Unit = {
    IO.delete(target) // delete from previous runs if clean wasn't called
    IO.touch(target)
    for (f <- files) {
      val content = IO.read(f)
      IO.append(target, content)
    }
  }


  def settings = {

    Seq[Setting[_]](
      jragSrcRoot := baseDirectory.value / "src" / "main" / "jrag",
      extendjRoot := jragSrcRoot.value / "extendj",
      java4 := extendjRoot.value / "java4",
      java5 := extendjRoot.value / "java5",
      java6 := extendjRoot.value / "java6",
      java7 := extendjRoot.value / "java7",
      java8 := extendjRoot.value / "java8",

      jrrtHome := jragSrcRoot.value / "jrrt",

      controlFlowGraph := jragSrcRoot.value / "ControlFlowGraph",

      puckJragJaddSrc := jragSrcRoot.value / "puck",

      jastaddOutDir := sourceManaged.value / "main",

      mainClass in Compile := Some("puck.Front"),
      (sourceGenerators in Compile) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue),
      (sourceGenerators in Test) ++= Seq(parser.taskValue, scanner.taskValue, ast.taskValue),

      cleanFiles += jastaddOutDir.value,

      parallelExecution in test := false, //cannot compile several program in parallel with jastadd

      //without this option, there is "cannot assign instance of scala.collection.immutable.List$SerializationProxy"
      // Cast exception raised in RecordingSerializationSpec ...
      //fork := true,
      astTask, parserTask, scannerTask
    )
  }

}
