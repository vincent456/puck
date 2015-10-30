package puck.intellij

import java.io.{PipedInputStream, PipedOutputStream, File}
import javax.swing.JPanel

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.{ProjectRootManager, JavaProjectRootsUtil}
import com.intellij.openapi.roots.ui.configuration.ModulesConfigurator
import com.intellij.openapi.wm.{ToolWindowManager, ToolWindow, ToolWindowFactory}
import com.intellij.ui.content.{Content, ContentManager}
import org.jetbrains.annotations.NotNull
import puck.graph.{NodeId, GraphUtils}
import puck.graph.io._
import puck.gui.svg.{SVGPanel, SVGController}
import puck.intellij.graphBuilding.IntellijGraphBuilder
import puck.javaGraph._

import scala.concurrent.ExecutionContext


object IntellijExecutionContext extends ExecutionContext {

  def execute(runnable: Runnable): Unit =
//    ApplicationManager.getApplication.invokeLater(runnable)
      ApplicationManager.getApplication.executeOnPooledThread(runnable)

  def reportFailure(cause: Throwable): Unit = {
    cause.printStackTrace()
    //Notifications.Bus notify new Notification("Puck", "", msg, NotificationType.ERROR)
  }

}


object IntellijSVGController {
  def builderFromModule
  ( module: Module,
    opts : PrintingOptions,
    graphUtils : GraphUtils,
    dg2ast : DG2AST ) : SVGController.Builder =
    ( frame : SVGPanel) =>
      new SVGController(graphUtils, dg2ast, frame,
        opts.visibility, opts.printId, opts.printSignatures,
        opts.printVirtualEdges, opts.printConcreteUsesPerVirtualEdges,
        opts.redOnly, opts.selectedUse){
        //val filesHandler: FilesHandler = filesHandler0

        lazy implicit val executor  = IntellijExecutionContext



        def deleteOutDirAndapplyOnCode() : Unit = {

        }

        def compareOutputGraph() : Unit = ???

        def workingDirectory : File = new File(module.getModuleFilePath)


        def swingInvokeLater (f : () => Unit ) : Unit =
          ApplicationManager.getApplication.invokeLater(new Runnable {
          def run(): Unit = f()
        })

      }
}


object PuckToolWindow {

  val id = "PuckToolWindow"

  var sToolWindow : Option[ToolWindow] = None

  var sSvgController : Option[SVGController] = None

  def createPanel(m : Module)  = {
    val builder = new IntellijGraphBuilder(m)
    val dg2ast = builder.build()
    val g = dg2ast.initialGraph

    val printingOptions =
      new PrintingOptions(defaultVisibilitySet(dg2ast),
        printId = true, printSignatures = true,
        redOnly = false, printVirtualEdges = false)

    val controlBuilder =
      IntellijSVGController.builderFromModule(m, printingOptions, JGraphUtils, dg2ast)

    val p = new SVGPanel(controlBuilder) with Runnable {
      def run() : Unit = this.revalidate()
    }
    sSvgController = Some(p.controller)
    p
  }

  def parseConstraint(file : File) : Unit =
    sSvgController foreach (_.parseConstraints(file))

  def setModule( m : Module) : Unit = sToolWindow foreach {
    toolWindow =>
    val panel = createPanel(m)

    val contentManager: ContentManager = toolWindow.getContentManager
    val content = contentManager.getFactory.createContent(panel, null, false)
    contentManager.removeAllContents(true)
    contentManager.addContent(content)

    toolWindow.activate(panel)
  }


  def defaultVisibilitySet(dg2ast: DG2AST) : VisibilitySet.T = {
    val g = dg2ast.initialGraph
    val s = VisibilitySet.allVisible(g)
    import VisibilitySet.VisibilitySetOps

    def f(s : VisibilitySet.T, rootName : String, includeRoot : Boolean) =
      dg2ast.nodesByName.get(rootName) map (r =>
        s.setVisibility(g.subTree(r, includeRoot), Hidden)) getOrElse s


    val s2 = f(s, "@primitive", includeRoot = true)
    f(s2, "java", includeRoot = false)
  }
}


class PuckToolWindowFactory extends ToolWindowFactory {

    def createToolWindowContent(@NotNull project: Project, @NotNull toolWindow: ToolWindow) {

//      val mconf = new ModulesConfigurator(project)
//      val modules = mconf.getModules

//      val prootManager = ProjectRootManager.getInstance(project)
//      prootManager.getContentSourceRoots

  //    modules(0).getModuleScope
  //    JavaProjectRootsUtil.isOutsideJavaSourceRoot()

      PuckToolWindow.sToolWindow = Some(toolWindow)

      val contentManager: ContentManager = toolWindow.getContentManager
      val content = contentManager.getFactory.createContent(new JPanel, null, false)
      contentManager.addContent(content)


    }
}
