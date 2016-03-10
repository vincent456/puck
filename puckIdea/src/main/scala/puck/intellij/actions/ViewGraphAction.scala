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
package actions

/**
 * Created by Loïc Girault on 29/10/15.
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
