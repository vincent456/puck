package puck.util

import puck.util.PuckLog.{Vanilla, Level, Kind, Info}

/**
 * Created by lorilan on 23/10/14.
 */
object PuckLog{

  type Verbosity = (Kind, Level)

  sealed abstract class Kind{
    def logString : String
  }
  case class Vanilla() extends Kind{
    def logString = ""
  }
  case class InGraph() extends Kind {
    def logString ="graph"
  }
  case class Solver() extends Kind{
    def logString = "solver"
  }
  case class Search() extends Kind{
    def logString = "search"
  }
  case class AG2AST() extends Kind{
    def logString = "ag2ast"
  }

  implicit val defaultVerbosity = (Vanilla(), Info())

  sealed abstract class Level{
    def logString : String
  }
  case class Info() extends Level{
    def logString = "info"
  }
  case class Warning() extends Level{
    def logString = "warning"
  }
  case class Error() extends Level{
    def logString = "error"
  }
  case class Debug() extends Level{
    def logString = "debug"
  }

}

trait PuckLogger extends LogBehavior[PuckLog.Verbosity]
                 with Logger[PuckLog.Verbosity]{

  val askPrint : PuckLog.Verbosity => Boolean

  implicit val defaultVerbosity = (Vanilla(), Info())

  def mustPrint(v : (Kind, Level)) = askPrint(v)

  def preMsg(v : (Kind, Level)) = v match {
    case (Vanilla(), lvl) => "[ " +lvl.logString + "] "
    case (k, lvl) => "[ %s - %s ] ".format(k.logString, lvl.logString)
  }
}

class PuckSystemLogger(val askPrint : PuckLog.Verbosity => Boolean )
  extends Logger[PuckLog.Verbosity] with PuckLogger {

  def writeln(msg : => Any)(implicit v : (Kind, Level)){
    if(mustPrint(v)) {
      println(preMsg(v) + msg)
    }
  }
  def write(msg : => Any)(implicit v : (Kind, Level)){
    if(mustPrint(v)) {
      print(preMsg(v) + msg)
    }
  }
}

object PuckNoopLogger extends PuckLogger {
  val askPrint = (x : PuckLog.Verbosity) => false
  def writeln(msg : => Any)(implicit v : PuckLog.Verbosity){}
  def write(msg : => Any)(implicit v : PuckLog.Verbosity){}
}