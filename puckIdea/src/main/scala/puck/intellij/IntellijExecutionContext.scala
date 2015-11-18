package puck.intellij

import com.intellij.openapi.application.ApplicationManager

import scala.concurrent.ExecutionContext

/**
  * Created by lorilan on 17/11/15.
  */
object IntellijExecutionContext extends ExecutionContext {

  def execute(runnable: Runnable): Unit =
//    ApplicationManager.getApplication.invokeLater(runnable)
      ApplicationManager.getApplication.executeOnPooledThread(runnable)

  def reportFailure(cause: Throwable): Unit = {
    cause.printStackTrace()
    //Notifications.Bus notify new Notification("Puck", "", msg, NotificationType.ERROR)
  }

}
