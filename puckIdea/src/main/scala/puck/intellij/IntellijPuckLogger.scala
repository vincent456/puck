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


package puck.intellij

import com.intellij.notification.{Notification, NotificationDisplayType, NotificationType, Notifications}
import puck.graph.io.FilesHandler
import puck.util.{PuckLog, PuckLogger}

object IntellijPuckLogger extends PuckLogger {
    def puck2IntellijLevel : PuckLog.Level => NotificationType = {
      case PuckLog.Info => NotificationType.INFORMATION
      case PuckLog.Warning => NotificationType.WARNING
      case PuckLog.Error => NotificationType.ERROR
      case PuckLog.Debug => NotificationType.INFORMATION
    }


  val askPrint : PuckLog.Verbosity => Boolean =  {
    case _ => true
  }

  Notifications.Bus.register("Puck", NotificationDisplayType.NONE)
  private def notify(msg : => String, v : PuckLog.Verbosity) : Unit =
    if(mustPrint(v)) v match {
      case (ctxt, lvl) =>
        Notifications.Bus notify new Notification("Puck", ctxt.toString, msg, puck2IntellijLevel(lvl))
    }

  def writeln(msg : => Any)(implicit v : PuckLog.Verbosity) =
      notify(msg.toString + System.lineSeparator(), v)

  def write(msg : => Any)(implicit v : PuckLog.Verbosity) =
    notify(msg.toString, v)

}