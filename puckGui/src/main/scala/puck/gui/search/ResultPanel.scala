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

//package puck.gui.search
//
//import puck.graph.constraints.search.RecordConstraintSolvingStateEvaluator
//import puck.graph.{SResult, DependencyGraph}
//import puck.graph.io.VisibilitySet
//import puck.graph.transformations.Transformation
//import puck.search.{SearchState, Search}
//import puck.util.{PuckLog, PuckLogger}
//
//import scala.swing._
//
//class ResultPanel
//( initialRecord : Seq[Transformation],
//  res : Search[SResult],
//  logger : PuckLogger,
//  printId : () => Boolean,
//  printSig: () => Boolean,
//  visibility : VisibilitySet.T)
//      extends BoxPanel(Orientation.Vertical){
//
//  implicit val defaultVerbosity : PuckLog.Verbosity = (PuckLog.NoSpecialContext, PuckLog.Info)
//
//  type ST = SearchState[DependencyGraph]
//
//  val evaluator = new RecordConstraintSolvingStateEvaluator(initialRecord)
//
//
//  logger.write("comparing final states : ")
//
//  val allStates = res.allStatesByDepth
//
//
////  val sortedRes: Map[Int, Seq[ST]] =
////    puck.util.Time.time(logger, defaultVerbosity){
////      evaluator.sortedDifferentStates(res.finalStates)
////    }
////  val total = sortedRes.foldLeft(0) { case (acc, (_, l)) => acc + l.size}
//
//
//  val sortedRes = Map(-1 -> res.successes)
//  val total = res.successes.size
//
//  logger.writeln("%d states explored".format(res.exploredStates))
//  logger.writeln("%d final states".format(res.successes.size))
//  logger.writeln("%d different final states ".format(total))
//
//  this deafTo this
//
//  reactions += {
//    case e => publish(e)
//  }
//
//  if(sortedRes.nonEmpty) {
//    //val comp : Component = new StateComparator(initialRecord, sortedRes, printId, printSig, visibility)
//    val comp0 : Component = new StateSelector(allStates, printId, printSig, visibility)
//    contents += comp0
//
//    this listenTo comp0
//
//
//    val comp : Component = new StateSelector(sortedRes, printId, printSig, visibility)
//    contents += comp
//
//    this listenTo comp
//
//
//    /*contents += new FlowPanel() {
//      val couplingValues = new ComboBox(sortedRes.keys.toSeq)
//      contents += couplingValues
//      contents += Button("Print") {
//        SearchResultPanel.this.
//          publish(SearchStateSeqPrintingRequest(couplingValues.selection.item.toString,
//          sortedRes(couplingValues.selection.item), None, printId(), printSig()))
//      }
//    }
//
//    contents += Button("Print all") {
//      SearchResultPanel.this.
//        publish(SearchStateMapPrintingRequest(sortedRes, printId(), printSig()))
//    }*/
//  }
//
//}
