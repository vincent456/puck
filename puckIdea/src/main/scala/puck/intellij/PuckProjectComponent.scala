package puck.intellij;

import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.{ToolWindowAnchor, ToolWindowManager, ToolWindow}

/**
 * Created by lorilan on 16/11/15.
 */
class PuckProjectComponent
(val project : Project) extends ProjectComponent {

    override def initComponent() : Unit = ()


    override def disposeComponent() : Unit = {
        // TODO: insert component disposal logic here
    }

    override def getComponentName : String = "PuckProjectComponent"


    override def projectOpened() : Unit = {
        PuckToolWindow.sToolWindow = Some(getToolWindow)
        PuckToolWindow.initWithEmptyPanel()
    }

    override def projectClosed() : Unit = {
        // called when project is being closed
    }

    private def getToolWindow: ToolWindow = {
        val toolWindowManager: ToolWindowManager = ToolWindowManager.getInstance(project)
        if (isToolWindowRegistered)
            toolWindowManager.getToolWindow(puckToolWindowId)
        else
            toolWindowManager.registerToolWindow(puckToolWindowId, true, ToolWindowAnchor.BOTTOM)
    }

    private def isToolWindowRegistered: Boolean =
        ToolWindowManager.getInstance(project).getToolWindow(puckToolWindowId) != null

}
