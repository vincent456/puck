package puck

import puck.javaGraph.JGraphUtils
import jastadd.JavaFilesHandler
object FrontVars {
  //val root = "/home/lorilan/puck_svn/examples/QualitasCorpus-20130901r/Systems"
  val root = "/home/lorilan/test_cases_for_puck/QualitasCorpus/Systems"

  //val system = "freecs/freecs-1.3.20100406"
  val system = "freemind/freemind-0.9.0"

  val workspace = s"$root/$system/puck_test"
}

object Front extends PuckApplication(
  JavaFilesHandler(FrontVars.workspace),
  JGraphUtils, JavaIcons)


/*
 from http://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli
 val usage = """
   Usage: mmlaln [--min-size num] [--max-size num] filename
             """
 def main(args: Array[String]) {
   if (args.length == 0) println(usage)
   val arglist = args.toList
   type OptionMap = Map[Symbol, Any]

   def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
     def isSwitch(s : String) = (s(0) == '-')
     list match {
       case Nil => map
       case "--max-size" :: value :: tail =>
         nextOption(map ++ Map('maxsize -> value.toInt), tail)
       case "--min-size" :: value :: tail =>
         nextOption(map ++ Map('minsize -> value.toInt), tail)
       case string :: opt2 :: tail if isSwitch(opt2) =>
         nextOption(map ++ Map('infile -> string), list.tail)
       case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
       case option :: tail => println("Unknown option "+option)
         exit(1)
     }
   }
   val options = nextOption(Map(),arglist)
   println(options)
 }
 */
