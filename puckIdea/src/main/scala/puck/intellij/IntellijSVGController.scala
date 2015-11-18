package puck.intellij

import java.io.File

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.module.Module
import puck.graph.GraphUtils
import puck.graph.io.{DG2AST, PrintingOptions}
import puck.gui.svg.{SVGPanel, SVGController}

/**
  * Created by lorilan on 17/11/15.
  */
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

        pushGraph(dg2ast.initialGraph)

        def deleteOutDirAndapplyOnCode() : Unit =
          dg2ast.apply(graph)


        def compareOutputGraph() : Unit = ???

        def workingDirectory : File = new File(module.getModuleFilePath)


        def swingInvokeLater (f : () => Unit ) : Unit =
          ApplicationManager.getApplication.invokeLater(new Runnable {
          def run(): Unit = f()
        })

      }
}
