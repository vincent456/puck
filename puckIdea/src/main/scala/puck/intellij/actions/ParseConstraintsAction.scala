package puck.intellij.actions

import java.io.File

import com.intellij.openapi.actionSystem.{AnActionEvent, AnAction}
import puck.intellij.PuckToolWindow

/**
 * Created by lorilan on 29/10/15.
 */
class ParseConstraintsAction extends AnAction {

  override def actionPerformed(event: AnActionEvent): Unit = {
    ActionEventUtil.getVirtualFile(event) foreach {
      vf =>
        PuckToolWindow.parseConstraint(new File(vf.getCanonicalPath))
    }

  }

}
