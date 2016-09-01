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

package puck.gui

import puck._
import java.awt.Dimension
import java.io.File
import javax.swing.filechooser.FileNameExtensionFilter

import puck.Project
import puck.config.{Config, ConfigWriter}
import puck.graph.{DecoratedGraph, DependencyGraph, Metrics}
import puck.graph.constraints.ConstraintsMaps
import puck.graph.constraints.search._
import puck.graph.io.{DotPrinter, VisibilitySet}
import puck.gui.svg.SVGViewHandler
import puck.gui.svg.actions.AutoSolveAction
import puck.piccolo.PiccoloViewHandler
import puck.search._

import scala.swing._
import scala.swing.SequentialContainer.Wrapper
import scala.swing.event.SelectionChanged



class PuckInterfacePanel
( control : PuckControl
) extends BoxPanel(Orientation.Vertical)  {

  private val publisher = control.Bus
  this listenTo control.Bus

  val leftWidth = PuckMainPanel.width * 3/8
  val rightWidth = PuckMainPanel.width * 5/8
  val height = PuckMainPanel.height * 2/3



  def makeButton(title:String, tip: String)(act:() => Unit): Component =
    new Button() {
      tooltip = tip
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize

      action = new Action(title) {
        def apply(): Unit = {
          act()
        }
      }
    }.leftGlued


  preferredSize = new Dimension(leftWidth, height)

  import control.sProject

  def addAlwaysVisibleButtons(): Unit ={
    contents += makeButton("Settings", "To set graphviz dot path"){
      () =>
        import Dialog._
        sProject match {
          case None => control.logger writeln "Create a project first"
          case Some(p) =>
            val ptmp = new Project(p.config, p.dG2ASTBuilder)
            Dialog.showConfirmation(parent = null,
              new SettingsPanel(ptmp).peer,
              title = "Settings",
              optionType = Options.OkCancel,
              messageType = Message.Plain) match {
              case Result.Ok =>
                //TODO !!
                val cfile = Config.defaultConfFile(p.workspace)
                ConfigWriter(cfile, ptmp.config)
                sProject = Some(ptmp)
                sProject.foreach {
                  p =>
                    DotPrinter.dotPath = p.graphvizDot.map(_.getAbsolutePath)
                }
                control.logger writeln s"New settings saved in ${cfile.getPath}"
              case _ =>
                control.logger writeln "New settings discarded"
            }
        }

    }

    contents += makeButton("Create project",
      "Select a workspace"){
      () =>
        val fc = new FileChooser(new File(".")){
          title = "What directory contains your application ?"
          fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
        }

        fc showDialog(null, "Select")

        Option(fc.selectedFile) foreach {
          workspace =>
            val fconf = Config.defaultConfFile(workspace)
            if (fconf.exists())
              control.logger writeln "Project already exists !"
            else {
              control.logger writeln "Creating default puck.xml"
              ConfigWriter(fconf, Config.defautlConfig(workspace))
              control.loadConf(fconf)
            }
        }

    }

    contents += makeButton("Load project",
      "Select a puck project file"){
      () =>
        val fc = new FileChooser(){
          title = "Select a puck project file"
          fileSelectionMode = FileChooser.SelectionMode.FilesOnly
          fileFilter = new FileNameExtensionFilter("Puck config file", "xml", "cfg")
        }

        fc showDialog(null, "Select")
        Option(fc.selectedFile) foreach {
          conffile =>
            if( sProject.isEmpty ||
              Config.defaultConfFile(sProject.get.workspace) != conffile )
              control.loadConf(conffile)
        }
    }

    contents += makeButton("(Re)load code & constraints",
      "Load the selected source code and build the access graph"){
      () => publisher publish LoadCodeRequest
    }
    ignore(contents += control.progressBar)
  }

  addAlwaysVisibleButtons()


  def addUndoRedoButton(c : Wrapper) : Unit = {
    import control.graphStack
    c.contents +=
      new Button(new Action("Undo all") {

        enabled = false

        def apply() = graphStack.undoAll()

        reactions += {
          case _ : GraphStackEvent =>
            enabled = graphStack.canUndo
        }
        listenTo(control.Bus)
      })

    c.contents +=
      new Button(new Action("Undo") {

        enabled = false

        def apply() = graphStack.undo()

        reactions += {
          case _ : GraphStackEvent =>
            enabled = graphStack.canUndo
        }
        listenTo(control.Bus)
      })

    ignore(c.contents +=
      new Button(new Action("Redo") {

        enabled = false

        def apply() = graphStack.redo()

        reactions += {
          case _ : GraphStackEvent =>
            enabled = graphStack.canRedo
        }
        listenTo(control.Bus)
      }))
  }

  def addLoadedGraphButtons(): Unit= {
    contents += makeButton("(Re)load constraints",
      "Decorate the graph with the constraints of the selected decouple file"){
      () => publisher publish LoadConstraintRequest
    }
    contents += makeButton("Show constraints",
      "Show the constraints the graph has to satisfy"){
      () => publisher publish ConstraintDisplayRequest(control.graph)
    }

    val p = new BoxPanel(Orientation.Horizontal)
    addUndoRedoButton(p)
    contents += p

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Button() {
        val b : Button = this
        action = new Action("Save refactoring plan") {

          def apply(): Unit = control.sProject foreach { p =>
            saveFile(p.workspace, b.peer) match {
              case None => publisher publish Log("no file selected")
              case Some(f) =>  publisher publish SaveRecord(f)
            }
          }
        }
      }

      contents += new Button() {
        val b : Button = this
        action = new Action("Load refactoring plan") {
          def apply(): Unit =  control.sProject foreach { p =>
            openFile(p.workspace, b.peer) match {
              case None => publisher publish Log("no file selected")
              case Some(f) => publisher publish LoadRecord(f)
            }
          }
        }
      }
    }.leftGlued

    contents += new ComboBox(List[ViewHandler](TreeViewHandler, SVGViewHandler, PiccoloViewHandler)) {
        minimumSize = new Dimension(leftWidth, 30)
        maximumSize = minimumSize
        preferredSize = minimumSize
        this listenTo selection

        reactions += {
          case SelectionChanged(_) =>
            publisher publish SwitchView(selection.item)
        }

      }


    type ControlBuilder = (DependencyGraph, ConstraintsMaps, VirtualNodePolicy) => SearchControl[DecoratedGraph[Any]]
    type StrategyBuilder = () => SearchStrategy[DecoratedGraph[Any]]


    import control.graphUtils
    val controlCB = new ComboBox[ControlBuilder](List(
      new ControlBuilder {
        override val toString = "Blind control"
        def apply(dg : DependencyGraph, cm : ConstraintsMaps, virtualNodePolicicy : VirtualNodePolicy) =
          new BlindControl(graphUtils.Rules, dg, cm,
            virtualNodePolicicy, graphUtils.violationsKindPriority).
            asInstanceOf[SearchControl[DecoratedGraph[Any]]]
      },
      new ControlBuilder {
        override val toString = "Control with Heuristic"
        def apply(dg : DependencyGraph, cm : ConstraintsMaps, virtualNodePolicicy : VirtualNodePolicy) =
          new ControlWithHeuristic(graphUtils.Rules, dg, cm, virtualNodePolicicy,
            graphUtils.violationsKindPriority).
            asInstanceOf[SearchControl[DecoratedGraph[Any]]]
      }
    )) {
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize
    }

    val kViolTextField = new TextField("1", 5)
    val kComplexTextField = new TextField("1", 5)

    val ponderationPanel = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Violation weight")
        contents += kViolTextField
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Complexity weight")
        contents += kComplexTextField
      }
    }

    val strategyCB =  new ComboBox(List[StrategyBuilder](
      new (() => SearchStrategy[DecoratedGraph[Any]]) {
        override val toString = "A* Strategy"
        def apply() = {
          Dialog.showConfirmation(message = ponderationPanel.peer)
          //function is called in search button action only if constraints is non empty
          val cm = control.constraints.get
          val kViol = kViolTextField.text.toInt
          val kComplex = kComplexTextField.text.toInt
          val f = Metrics.fitness1(_ : DependencyGraph , cm, kViol, kComplex).toDouble
          new AStarSearchStrategy(DecoratedGraphEvaluator.equalityByMapping(f))
        }


      },
    new (() => SearchStrategy[DecoratedGraph[Any]]) {
        override val toString = "Depth-first Strategy"
        def apply() = new DepthFirstSearchStrategy()
      },
    new (() => SearchStrategy[DecoratedGraph[Any]]) {
        override val toString = "Breadth-first Strategy"
        def apply() = new BreadthFirstSearchStrategy()
      }
    )) {
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize
    }

    val vnStrategyCB =  new ComboBox(List[VirtualNodePolicy](WithVirtualNodes,NoVirtualNodes)) {
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize
    }

    contents += controlCB
    contents += strategyCB
    contents += vnStrategyCB
    contents += makeButton("Search", ""){
      () =>
        control.constraints foreach {
          cm =>
            val s = strategyCB.selection.item()
            val vns = vnStrategyCB.selection.item
            val c = controlCB.selection.item(control.graph.newGraph(mutabilitySet = control.mutabilitySet), cm, vns)
            Swing onEDT new AutoSolveAction(control.Bus, cm, control.printingOptionsControl,
              s, c)(control.graphUtils, control.nodeKindIcons).apply()
        }

    }

    contents += makeButton("Show recording", ""){
      control.printRecording
    }

    contents += makeButton("Focus on Violations",
      "Show only the nodes involved in a constraint violation"){
      () =>
        val vs = control.constraints match {
          case None => VisibilitySet.allHidden(control.graph)
          case Some(cm) => VisibilitySet.violationsOnly(control.graph, cm)
        }

        publisher publish VisibilityEvent(control.graph, vs)

    }

    contents += new Button() {
      val b : Button = this
      action = new Action("Export Graph") {

        def apply(): Unit = control.sProject foreach { p =>

          val sd = p.outDirectory match {
            case Some(d) =>
              if(!d.exists())
                d.mkdirs()
              Some(d)
            case None =>
              val fc = new FileChooser(){
                title = "Select a folder where to export the graph"
                fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
              }
              fc showDialog(null, "Select")
              Option(fc.selectedFile)
          }

          sd foreach {
            f => publisher publish ExportGraph(f)
          }
        }
      }
    }

    val testCommutativityCB = new CheckBox("Test commutativity")

    contents += testCommutativityCB

    ignore(contents += makeButton("Generate Code",
      "Apply transformations on the code")(
      () =>publisher publish
        GenCode(compareOutput = testCommutativityCB.selected)))
  }


  reactions += {
    case GraphUpdate(_)
         | ConstraintsUpdate(_,_) =>
      contents.clear()
      addAlwaysVisibleButtons()
      addLoadedGraphButtons()
      revalidate()
  }


}
