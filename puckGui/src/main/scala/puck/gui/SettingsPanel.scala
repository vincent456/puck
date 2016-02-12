package puck.gui

import java.awt.geom.{Area, RoundRectangle2D}
import java.awt.{Color, RenderingHints, BasicStroke}
import javax.swing.border.AbstractBorder
import javax.swing.{ImageIcon, Icon}

import puck.Project
import puck.config.Config
import puck.config.Config._

import scala.swing._
import scala.swing.event.MouseClicked

import puck.util.FileHelper.FileOps
abstract class SettingLinePanel(val text : String, img : java.net.URL)
  extends BoxPanel(Orientation.Horizontal){
  def action(mc : MouseClicked) : Unit
  contents += new Label(text)
  contents += Swing.HGlue
  contents += new Label {
    icon = new ImageIcon(img)
    listenTo(mouse.clicks)
    reactions += {
      case mc@MouseClicked(_, _, _, _, _) => action(mc)
    }
  }
}

object SettingsPanel {
  val addimg = getClass.getResource("/icons/add.png")
  val deleteimg = getClass.getResource("/icons/delete.png")
  val editimg = getClass.getResource("/icons/edit.png")
}
import SettingsPanel._
class FileKeyPanel(project : Project, k : FileKey)
  extends BoxPanel(Orientation.Vertical) {

  val path : Label = new Label(project(k) match {
    case None => ""
    case Some(SingleFile(f)) => f
  })

  border = new BubbleBorder(Color.BLACK, 1, 8)
  contents += new SettingLinePanel(k.v, editimg) {
    def action(mc : MouseClicked) : Unit = {
      val fc = new FileChooser(project.workspace){
        title = k.v
        fileSelectionMode = FileChooser.SelectionMode.FilesOnly
      }
      fc showDialog(null, "Select")
      Option(fc.selectedFile) foreach {
        f =>
          val ff = SingleFile(f.pathRelativeTo(project.workspace))
          project set (k, ff)
          path.text = ff.path
      }
     }
  }
  contents += new Separator()
  contents += path

}

class FileListSelectionPanel(project : Project, k : FileListKey)
  extends BoxPanel(Orientation.Vertical) {
  panel : FileListSelectionPanel =>

  border = new BubbleBorder(Color.BLACK, 1, 8)

  def selectSuffix : Option[String] =
    Dialog.showInput[String](
      message = "Select suffix",
      title = "Suffix",
      entries = Seq("None", ".java", ".jar"),
      initial = ".java")

  contents += new SettingLinePanel(k.v, addimg) {
    def action(mc : MouseClicked) : Unit =
      if(!isRightClick(mc.peer)){
        val fc = new FileChooser(project.workspace)
        fc.title = k.v
        fc.fileSelectionMode = FileChooser.SelectionMode.FilesAndDirectories
        fc showDialog(null, "Select")

        val sff: Option[FileFinder] = fc.selectedFile match {
          case null => None
          case f if !f.isDirectory =>
            Some(SingleFile(f.pathRelativeTo(project.workspace)))
          case f =>
            selectSuffix match {
              case None
                   | Some("None")=>
                Some(SingleFile(f.pathRelativeTo(project.workspace)))
              case Some(suffix) =>
                Some(Root(f.pathRelativeTo(project.workspace), suffix))
            }
        }
        sff foreach {
          ff =>
            project add(k, ff)
            panel add ff
            panel.revalidate()
        }
      }

  }
  contents += new Separator()

  def add(ff : FileFinder) : Unit =
    contents += new SettingLinePanel(ff.path, deleteimg) {
      def action(mc : MouseClicked) : Unit =
        if(!isRightClick(mc.peer)){
          project.remove(k, ff)
          panel.contents -= this
          panel.revalidate()
        }
    }

  project.config getOrElse (k, List()) foreach add


}

class SettingsPanel(project : Project)
  extends BoxPanel(Orientation.Vertical){

  Config.listValueKeys foreach (
    contents += new FileListSelectionPanel(project, _))

  Config.singleValueKeys foreach (
    contents += new FileKeyPanel(project, _))

}

import java.awt.{Component, Graphics}

class BubbleBorder
( color: Color,
  thickness : Int,
  radii : Int ) extends AbstractBorder {

  val stroke = new BasicStroke(thickness.toFloat)
  val strokePad = thickness / 2
  val hints = new RenderingHints (
    RenderingHints.KEY_ANTIALIASING,
    RenderingHints.VALUE_ANTIALIAS_ON)

  val insets : Insets = {
    val pad = radii + strokePad
    new Insets(pad, pad, pad, pad)
  }


  def this(color : Color) =  this(color, 4, 8)

  override def getBorderInsets(c : Component) : Insets= insets

  override def getBorderInsets(c : Component, insets: Insets) : Insets =
    getBorderInsets(c)

  override def paintBorder(c : Component, g : Graphics,
                           x : Int, y : Int, width : Int, height : Int) = {

    val g2 = g.asInstanceOf[Graphics2D]

    val bottomLineY = height - thickness

    val bubble = new RoundRectangle2D.Double(
      0 + strokePad,
      0 + strokePad,
      width - thickness,
      bottomLineY,
      radii,
      radii)

    g2.setRenderingHints(hints)

    // Paint the BG color of the parent, everywhere outside the clip
    // of the text bubble.
    Option(c.getParent).foreach {
      parent =>
        val bg = parent.getBackground
        val rect = new Rectangle(0,0,width, height)
        val borderRegion = new Area(rect)
        g2.setClip(borderRegion)
        g2.setColor(bg)
        g2.fillRect(0, 0, width, height)
        g2.setClip(null)
    }

    g2.setColor(color)
    g2.setStroke(stroke)
    g2.draw(bubble)
  }
}