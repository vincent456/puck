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
class GenDependencyGraph
(val global: Global) extends PluginComponent with GraphBuilder {
  import global._

  val root = ConcreteNode(rootId, rootName, ScalaRoot, None, mutable = true)

  g = new DependencyGraph(ScalaNodeKind,
    NodeIndex(root), EdgeMap(),
    AbstractionMap(), ConstraintsMaps(), Recording())

  def updateGraph(newG : DependencyGraph) = g = newG

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
        addTree(g.rootId)(cu.body)
//        println(cu.body)
      }

    }

    def typeOf(valOrDefDef: ValOrDefDef): PuckType =
      PuckNamedType(0)

    //      valOrDefDef match {
    //        case DefDef(_, _, _, _, _, _) =>
    //        case ValDef(mods, _, _, _) =>
    //      }

    def addDef(container: NodeId, defTree: DefTree): NodeId = {
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
      val (cn, g2) = g.addConcreteNode(defTree.name.toString, k, styp, mutable = true)
      updateGraph(g2.addContains(container, cn.id))
      cn.id
    }


    def addTree(container: NodeId)(t: Tree) : Unit = {
      val cid = t match {
        case dt : DefTree => addDef(container, dt)
        case _ => container
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
      t.children foreach addTree(cid)
    }
  }

}

class PuckPlugin(val global: Global) extends Plugin {

  val name: String = "PuckPlugin"
  val description: String = "Dependency Graph Generator for Puck"
  val genGraphComponent = new GenDependencyGraph(global)
  val components: List[PluginComponent] = List(genGraphComponent)

}
