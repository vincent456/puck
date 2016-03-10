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

import puck.graph.{Type => PuckType, NamedType => PuckNamedType, _}
import puck.graph.constraints.ConstraintsMaps
import puck.scalaGraph.nodeKind.{Type => TypeKind, _}

import scala.reflect.internal.Flags
import scala.tools.nsc
import scala.tools.nsc.{Global, Phase}
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

import DependencyGraph._

class DummyPrintingComponent
(val global: Global,
 val prevPhase : String) extends PluginComponent  {

  import global._

  override val runsRightAfter: Option[String] = Some(prevPhase)
  val runsAfter: List[String] = List(prevPhase)

  override val phaseName: String = "DummyPrinting"

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    override def run() = {
      println(s"Printing after $prevPhase :")
      currentRun.units foreach { cu =>
        println(cu.body)
      }

    }
    def apply(unit: CompilationUnit) = {
      assert(false)
    }
  }

}

class GenDependencyGraph
(val global: Global) extends PluginComponent with GraphBuilder {
  import global._

  val root = ConcreteNode(rootId, rootName, ScalaRoot, mutable = true)

  g = new DependencyGraph(ScalaNodeKind,
    NodeIndex(root), EdgeMap(),
    AbstractionMap(), ConstraintsMaps(), Recording())

  val prevPhase = PuckPluginSettings.prevPhase

  override val runsRightAfter: Option[String] = Some(prevPhase)
  val runsAfter: List[String] = List(prevPhase)
  /** Useful for -Xshow-phases. */
  override def description: String = PuckPluginSettings.description
  val phaseName: String = PuckPluginSettings.phaseName

  def newPhase(prev: Phase): Phase = new StdPhase(prev) {
    // apply is called for each file, but we want to run once for all files, that's why we override run later on.
    def apply(unit: CompilationUnit) = {
      assert(assertion = false)
    }

    override def run() = {
      global.currentRun.units.foreach { cu =>
        addTree(g.rootId, "")(cu.body)
        println(cu.body)
      }

    }

    def typeOf(valOrDefDef: ValOrDefDef): PuckType =
      PuckNamedType(0)

    //      valOrDefDef match {
    //        case DefDef(_, _, _, _, _, _) =>
    //        case ValDef(mods, _, _, _) =>
    //      }

    def addDef(container: NodeId, acc : String, defTree: DefTree): (NodeId, String) = {
      import Flags.{TRAIT, MUTABLE}

      val (k, styp) =
        defTree match {
          case TypeDef(_, _, _, _) => (TypeKind, None)
          case ClassDef(mods, _, _, _) =>
            (if (mods hasFlag TRAIT) Trait else Class, None)
          case dd@DefDef(_, _, _, _, _, _) => (Def, Some(typeOf(dd)))
          case ModuleDef(_, _, _) => (Object, None)
          case PackageDef(_, _) => (Package, None)
          case vd@ValDef(mods, _, _, _) =>
            (if (mods hasFlag MUTABLE) Var else Val, Some(typeOf(vd)))
        }
      val fullName = acc + "." + defTree.name
      val nid = addNode(fullName, defTree.name.toString, k, mutable = true)
      addContains(container, nid)
      styp foreach (setType(nid,_))

      (nid, fullName)
    }


    def addTree(container: NodeId, acc: String)(t: Tree) : Unit = {
      println("t.hasExistingSymbol " + t.hasExistingSymbol)
      println("t.pos.isRange " + t.pos.isRange)

      if(t.hasExistingSymbol && !t.pos.isRange) { //TODO - TOCHECK : inSource <=> !isRange ???
        val (cid, newAcc) = t match {
          case dt: DefTree => addDef(container, acc, dt)

          case _ => (container, acc)
          //      t match {
          //        case pd @ PackageDef(pid, stats) =>
          //          stats foreach (addTree(_, cn.id))
          //        case cls @ ClassDef(mods, _, _, _)  =>
          //        case TypeDef(_, _, _, _)      => "type"
          //        case DefDef(_, _, _, _, _, _) => "def"
          //        case ModuleDef(_, _, _)       => "object"
          //        case ValDef(mods, _, _, _)    =>
          //        case _ =>
          //      }
        }
        t.children foreach addTree(cid, newAcc)
      }
    }
  }

}

class PuckPlugin(val global: Global) extends Plugin {

  val name: String = "PuckPlugin"
  val description: String = "Dependency Graph Generator for Puck"
  val genGraphComponent = new GenDependencyGraph(global)

  val components: List[PluginComponent] = List(genGraphComponent,
    new DummyPrintingComponent(global, "parser"))
}
