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

import java.io.File

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.module.Module
import puck.graph.GraphUtils
import puck.graph.io.{DG2AST, PrintingOptions}
import puck.view.svg.{SVGPanel, SVGController}

/**
  * Created by Loïc Girault on 17/11/15.
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
