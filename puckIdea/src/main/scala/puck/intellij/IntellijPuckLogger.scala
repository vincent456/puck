
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