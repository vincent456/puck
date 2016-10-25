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

package puck

import java.io.File

import puck.graph.comparison.Mapping
import puck.graph.transformations.{Recordable, Recording}
import puck.util.{PuckLogger, PuckSystemLogger}
import Recording.RecordingOps
import org.extendj.JavaProject
import org.extendj.ast.JavaJastAddDG2AST
import puck.graph.DependencyGraph


object LoadAndApply  {

  def applyRecords
  ( p : Project,
    recs : Seq[String],
    testCommutativity : Boolean = false)
  ( implicit logger : PuckLogger ) : Project = {


    val dg2ast = p.loadGraph()

    val g = recs.foldLeft(dg2ast.initialGraph) {
      case (g0, recName) =>
        val r = Recording.load(new File(recName).getAbsolutePath, dg2ast.nodesByName  )
        r.redo(g0)
    }

    import ProjectDG2ASTControllerOps.deleteOutDirAndApplyOnCode

    deleteOutDirAndApplyOnCode(dg2ast, p, g, p.parseConstraints(dg2ast))

    if (testCommutativity) {
      p.fromOutDir foreach {
        outProject =>
          val dg2astout = outProject.loadGraph()

          val gout = dg2astout.initialGraph

          try {
            if (Mapping.equals(g, gout)) {
              println("ARE equals")
            }
            else {
              println("are NOT equals")
              import puck.graph.ShowDG._

              (g, g.nodesIndex).println
              (gout, gout.nodesIndex).println
            }
          } catch {
            case e: PuckError => println(e.getMessage)
          }
      }
    }

    p
  }


  def applyRecursivelyStepByStep
  ( p : Project,
    recFileName : String )
  (implicit logger : PuckLogger) : Project = {

    val dg2ast = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]
    //val scm = p.parseConstraints(dg2ast)

    val r = Recording.load(recFileName, dg2ast.nodesByName  )

    val splittedRec = r.splitAtMilestones

    def applyRec(g: DependencyGraph, i : Int, recs : Seq[Seq[Recordable]]) : DependencyGraph =
      if(i == 0 || recs.isEmpty) g
      else applyRec(recs.head.redo(g), i - 1, recs.tail)


    p.outDirectory foreach {
      outDirectory =>
        for(i <- splittedRec.indices) {
          println("***********************************************")
          println("***********************************************")
          println("***********************************************")
          println("***********************************************")
          println("STEP " + i)
          println("***********************************************")
          println("***********************************************")

          val dg2ast = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]
          val g = applyRec(dg2ast.initialGraph, i, splittedRec)
          dg2ast(g)
          dg2ast.printCode(outDirectory)
        }
        println( outDirectory.getAbsolutePath)
    }
    p
  }


  def main (args: Array[String]) : Unit = {
    this.applyRecords(JavaProject(), args.toSeq)(new PuckSystemLogger(_ => true))
  }



}
