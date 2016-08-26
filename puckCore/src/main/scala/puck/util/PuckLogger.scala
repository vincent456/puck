/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

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