package puck.intellij
package actions

/**
 * Created by lorilan on 29/10/15.
 */

import com.intellij.openapi.actionSystem._
import com.intellij.openapi.wm.ToolWindowManager

import com.intellij.psi.PsiDocumentManager
import puck.intellij.PuckToolWindow




class ViewGraphAction extends AnAction {

  override def actionPerformed(event: AnActionEvent): Unit = {
    val presentation = event.getPresentation
    val project = ActionEventUtil.getProject(event)

    PsiDocumentManager.getInstance(project).commitAllDocuments()

    if (project == null)
    { // project isn't accessible from the context
      presentation.setEnabled(false)
      presentation.setVisible(false)
      return
    }

    val toolWindow = ToolWindowManager.getInstance(project).getToolWindow(puckToolWindowId)

    if (toolWindow == null)
    { // tool window isn't registered
      presentation.setEnabled(false)
      presentation.setVisible(false)
      return
    }

    ActionEventUtil.getModule(event) foreach PuckToolWindow.setModule



  }

}
