package puck.util

import puck.util.PuckLog.{NoSpecialContext, Level, Kind, Info}

object PuckLog{

  type Verbosity = (Kind, Level)

  val verbose : Verbosity => Boolean = _ => true

  sealed abstract class Kind{
    def logString : String
  }
  case object NoSpecialContext extends Kind{
    def logString = ""
  }
  case object InGraph extends Kind {
    def logString = "graph"
  }
  case object GraphTransfoRules extends Kind {
    def logString = "transfoRules"
  }
  case object InJavaGraph extends Kind {
    def logString = "JavaGraph"
  }
  case object Solver extends Kind{
    def logString = "solver"
  }

  case object GraphComparisonSearch extends Kind{
    def logString = "node mapping"
  }

  case object ConstraintSearch extends Kind{
    def logString = "search"
  }
  case object AG2AST extends Kind{
    def logString = "ag2ast"
  }

  implicit val defaultVerbosity = (NoSpecialContext, Info)

  sealed abstract class Level{
    def logString : String
  }
  case object Info extends Level{
    def logString = "info"
  }
  case object Warning extends Level{
    def logString = "warning"
  }
  case object Error extends Level{
    def logString = "error"
  }
  case object Debug extends Level{
    def logString = "debug"
  }

}

trait PuckLogger extends LogBehavior[PuckLog.Verbosity]
                 with Logger[PuckLog.Verbosity]{

  val askPrint : PuckLog.Verbosity => Boolean

  implicit val defaultVerbosity = (NoSpecialContext, Info)

  def mustPrint(v : (Kind, Level)) = askPrint(v)

  def preMsg(v : (Kind, Level)) : String = v match {
    case (NoSpecialContext, lvl) => "[ " +lvl.logString + "] "
    case (k, lvl) => "[ %s - %s ] ".format(k.logString, lvl.logString)
  }
}

class PuckSystemLogger(val askPrint : PuckLog.Verbosity => Boolean )
  extends SystemLogger[PuckLog.Verbosity] with PuckLogger {

  override def writeln(msg : => Any)(implicit v : (Kind, Level)) =
    super.writeln(preMsg(v) + msg)(v)

  override def write(msg : => Any)(implicit v : (Kind, Level))=
    super.write(preMsg(v) + msg)(v)
}

class PuckFileLogger(val askPrint : PuckLog.Verbosity => Boolean,
                     val file : java.io.File) extends FileLogger[PuckLog.Verbosity] with PuckLogger {

  override def writeln(msg : => Any)(implicit v : (Kind, Level)) =
    super.writeln(preMsg(v) + msg)(v)

  override def write(msg : => Any)(implicit v : (Kind, Level))=
    super.write(preMsg(v) + msg)(v)
}

object PuckNoopLogger extends PuckLogger {
  val askPrint = (x : PuckLog.Verbosity) => false
  def writeln(msg : => Any)(implicit v : PuckLog.Verbosity): Unit = {}
  def write(msg : => Any)(implicit v : PuckLog.Verbosity): Unit = {}
}