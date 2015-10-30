package puck.intellij.actions

/**
 * Created by lorilan on 29/10/15.
 */

import com.intellij.openapi.actionSystem._

import com.intellij.psi.PsiDocumentManager
import puck.intellij.PuckToolWindow




class ViewGraphAction extends AnAction {

  override def actionPerformed(event: AnActionEvent): Unit = {
    val project = ActionEventUtil.getProject(event)

    PsiDocumentManager.getInstance(project).commitAllDocuments()

    ActionEventUtil.getModule(event) foreach PuckToolWindow.setModule

  }

}
