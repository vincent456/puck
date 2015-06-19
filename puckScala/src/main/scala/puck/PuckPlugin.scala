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

  val root = ConcreteNode(rootId, rootName, ScalaRoot, None, mutable = true)

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
      val nid = addNode(fullName, defTree.name.toString, k, styp)
      addContains(container, nid)

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
