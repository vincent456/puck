
import sbt._
import Keys._

object JrrtTasks extends Build {


  val jrrtHome = settingKey[File]("Jrrt build directory")

  val jrrtReadOnly = settingKey[File]("jrrt-read-only directory")

  val java14frontend = settingKey[File]("Java1.4Frontend directory")

  val java15frontend = settingKey[File]("Java1.5Frontend directory")

  val controlFlowGraph = settingKey[File]("ControlFlowGraph directory")

  val java15comply = settingKey[Boolean]("will produce parser and scanner for java1.5 ")

  val jastaddSrcDir = settingKey[File]("Location of puck jrag files")

  /*
    Tasks
   */

  val ast = taskKey[Seq[File]]("use ast, jrag and jadd files to generates java")
  val parser = taskKey[Seq[File]]("create java parser")
  val scanner = taskKey[Seq[File]]("create java scanner")

/*
  val gen = taskKey[Unit]("generates parser, scanner and AST files")
*/


  def concat(target: File, files : Seq[File]): Unit ={
    IO.delete(target) // delete from previous runs if clean wasn't called
    IO.touch(target)
    for (f <- files) {
      val content = IO.read(f)
      IO.append(target, content)
    }
  }

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
          error("Error while generating parser")
        }
        

      } catch {
          case e : Exception => error(e.getMessage())
      }
  }

  

  lazy val root =
	    Project(id = "PuckConstraintSolver", base = file("."))
	      .settings(

        parser := {

          //helper function to avoid a call to beaver main method that uses System.exit
  
          (sourceManaged.value / "parser").mkdirs()
          val parserAll = sourceManaged.value / "parser" / "JavaParser.all"
          /* generate the parser phase 1, create a full .lalr specification from fragments */

          val java15Files = if(java15comply.value)
            (PathFinder(java15frontend.value) * "*.parser").get
            else
          Seq()


          concat(parserAll,
          	Seq(jrrtReadOnly.value / "util" / "preamble.parser",
          		java14frontend.value / "parser" / "java14.parser",
          		java14frontend.value / "parser" / "errorrecovery.parser" ) ++ java15Files )

          /* generate the parser phase 2, translating .lalr to .beaver */
          // val jastAddPaserJar = baseDirectory.value / "lib" / "JastAddParser.jar"
          // val beaverRtJar = baseDirectory.value / "lib" / "beaver-rt.jar"
          val javaParserBeaver = sourceManaged.value / "parser" / "JavaParser.beaver"
          // Process(Seq("java", "-cp", jastAddPaserJar.getPath + ":" + beaverRtJar.getPath,
          //   "Main", parserAll.getPath,  javaParserBeaver.getPath) ).!

          //Main class of JastAddParser.jar
          Main.main(Array(parserAll.getPath,  javaParserBeaver.getPath))

          //val beaverAnt = baseDirectory.value / "lib" / "beaver-cc-0.9.11.jar"
          /* generate the parser phase 3, translating .beaver to .java */
          //Process(Seq("java", "-jar",  beaverAnt.getPath, "-t", "-w", javaParserBeaver.getPath )).!
          //beaver.comp.run.Make.main(Array("-t", "-w", javaParserBeaver.getPath))
          beaverTask(javaParserBeaver)

         Seq(sourceManaged.value / "parser" / "JavaParser.java")
        },

      scanner := {

        val scannerFlex = sourceManaged.value / "scanner" / "JavaScanner.flex"

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

        concat(scannerFlex, files)

        jflex.Main.generate(Array("--nobak",
          "--legacydot",
          "-d", (sourceManaged.value / "scanner").getPath,
          scannerFlex.getPath))

        IO.copyFile(java14frontend.value / "scanner" / "Unicode.java",
          sourceManaged.value / "scanner" / "Unicode.java",
          preserveLastModified = true)

        Seq(sourceManaged.value / "scanner" / "JavaScanner.java",
          sourceManaged.value / "scanner" / "Unicode.java")
      },

	    ast := {

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
                jrrtDir / "TypeConstraints" / "CollectTypeConstraints.jrag",
                jrrtDir / "MakeMethodStatic" / "MakeMethodStatic.jrag",
                jrrtDir / "ChangeMethodSignature" / "ChangeParameterType.jrag")

          val puckFiles = PathFinder(jastaddSrcDir.value) ** ("*.jrag" | "*.jadd")

          val jrrtFiles3 : PathFinder = Seq(jrrtDir / "TypeConstraints" / "TypeConstraintSolving.jrag",
            jrrtDir / "TypeConstraints" / "CollectTypeConstraints.jrag")

          val paths = (java14Files.getPaths
            ++: java15Files.getPaths.sorted
            ++: cfgFiles.getPaths
            ++: jrrtUtilFiles.getPaths.sorted
            ++: jrrtFiles2.getPaths.sorted
            ++: puckFiles.getPaths
            //++: jrrtFiles3.getPaths

            )

          // Fork.java(new ForkOptions(), 
          //       "jastadd.JastAdd.main" 
          //         +: "--beaver"
          //         +: "--package=AST"
          //         +: ("--o=" + sourceManaged.value)
          //         +: "--rewrite"
          //         +: "--novisitcheck"
          //         +: "--noCacheCycle"
          //         +: "--noComponentCheck"
          //         +: "--refineLegacy"
          //         +: paths )

          // /!\ breakable : main uses System.exit !!
          jastadd.JastAdd.main(("--beaver"
            +: "--package=AST"
            +: ("--o=" + sourceManaged.value)
            +: "--rewrite"
            +: "--novisitcheck"
            +: "--noCacheCycle"
            +: "--noComponentCheck"
            +: "--refineLegacy"
            +: paths ).toArray)

       IO.copyDirectory(java14frontend.value / "beaver",
         sourceManaged.value / "beaver",
         preserveLastModified = true)
        (PathFinder( sourceManaged.value / "beaver" ) * "*" +++ PathFinder(sourceManaged.value / "AST") * "*").get
			}
		)

}