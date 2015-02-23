/*
package puck.gui.search.decisionsFrames

import puck.graph.constraints._
import puck.graph.DGNode

import scala.swing._
import java.awt.Color
import scala.collection.mutable


/**
 * Created by lorilan on 10/06/14.
 */

object ConstraintEditor {
  def separator() =
    new BoxPanel(Orientation.Vertical) {
      contents += new Label(", ")
      contents += Swing.VGlue
    }
}

abstract class ConstraintEditor protected
( val constraint : Constraint,
  var users : RangeSet,
  val usee : DGNode,
  finish : () => Unit)
  extends BoxPanel(Orientation.Vertical) {

  /*  def init() : Unit
  def onReload() : Unit
  def reload(){
    onReload()
    init()
    revalidate()
  }

  def init(elts : Iterator[Component]) {
    contents.clear()

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new BoxPanel(Orientation.Vertical) {
        contents += new Label(constraint.predicate + "(")
        contents += Swing.VGlue
      }

      elts.foreach{contents.+=}

      contents += new BoxPanel(Orientation.Vertical) {
        contents += new Label(").")
        contents += Swing.VGlue
      }
    }
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += Button("Raw Edit") {
        println("not implemented !")
      }
      contents += Button("OK") {
        finish()
      }
    }
  }

  init()*/

}


class FriendPanel(val editor : ConstraintEditor)
  extends BoxPanel(Orientation.Vertical){

  val nsd: RangeSetDiff = ??? //= new RangeSetDiff(users, LiteralRangeSet())
  ???
  /*
  import editor.constraint
  import editor.users
  import editor.usee

    constraint.scopeFriends.foreach { f =>
    contents += (users.scopeThatContains_*(f) match {
      case Some(s) =>
        nsd -= s
        new ComboBox(f.containerPath(s))
      case None => new Label(f.toString)
    })
  }
  nsd.foreach { u =>
    constraint.interlopers.scopeThatContains_*(u) match{

      case Some(s) if s != u =>
        contents += Button("add " + u) {
          constraint.friends += u
          editor.reload()
        }
      case _ => ()
    }


  }*/

  if (contents.isEmpty)
    contents += new Label("[]")

  contents+= Swing.VGlue

}

class InterlopersPanel(val editor : ConstraintEditor)
  extends BoxPanel(Orientation.Vertical){
  ???
  /*
  import editor.constraint
  import editor.users
  import editor.usee

    constraint.interlopers.foreach{ n =>

    val c  = users.find(n.contains_*) match {
      case Some(s) if s == n => Button("remove " + n){
        constraint.interlopers -= n
        editor.reload()
      }

      case Some(s) =>
        val l = new Label(n.toString)
        l.foreground = Color.RED
        l
      case None => new Label(n.toString)
    }

    contents += c
  }*/
  contents+= Swing.VGlue
}

class OwnerPanel(val editor : ConstraintEditor)
  extends BoxPanel(Orientation.Vertical){

  ???
 /*
  import editor.constraint
  import editor.users
  import editor.usee

  constraint.owners.foreach{ n =>

    val c =
    if(n.contains_*(usee)) {
      if (n != usee) {
        val l = new Label(n.toString)
        l.foreground = Color.RED
        l
      }
      else
        Button("remove " + n){
          constraint.owners -= usee
          usee.remove(constraint)
          editor.reload()
        }
    }
    else
      new Label(n.toString)

    contents += c
  }*/
  contents+= Swing.VGlue
}

object ScopeConstraintEditor{
  def apply(constraint : Constraint,
            users : RangeSet,
            usee : DGNode,
            finish : () => Unit) = new ScopeConstraintEditor(constraint, users, usee, finish)
}

class ScopeConstraintEditor private (constraint0 : Constraint,
                                     users0 : RangeSet,
                                     usee0 : DGNode,
                                     finish0 : () => Unit)
  extends ConstraintEditor(constraint0, users0, usee0, finish0) {

  def onReload() : Unit = {
    val set = LiteralRangeSet()
    ???
    /*users.foreach{ u =>
      if(constraint.isViolatedBy(AGEdge.uses(u, usee))){
        set += u
      }
    }*/
    users = set
  }

  def init() : Unit = {
    val contents = mutable.Buffer[Component]()

    contents += new OwnerPanel(this)

    contents += ConstraintEditor.separator();()

    /**
     * Facade
     */
    contents += new BoxPanel(Orientation.Vertical) {

      var needNewCell = true
      ???
      /* constraint.facades.foreach { f =>
        val c =
          if (usee.contains_*(f)){
            needNewCell = false
            new ComboBox[AGNode[K]](f.containerPath(usee))
          }
          else
            new Label(f.toString)
        contents += c
      }
      if(needNewCell){
        constraint.owners.scopeThatContains_*(usee) match {
          case None => throw new AGError("why a violation then ?")
          case Some(s) => if(s!= usee){
            contents += new ComboBox(usee.containerPath(s).tail)
          }
        }
      }
      if(contents.isEmpty){
        contents += new Label("[]")
      }
      contents+= Swing.VGlue
    }
    contents += ConstraintEditor.separator()
    contents += new InterlopersPanel(this)
    contents += ConstraintEditor.separator()
    contents += new FriendPanel(this)
    init(contents.iterator)*/
    }
    ()
  }
}
object ElementConstraintEditor{
  def apply(constraint : Constraint,
            users : RangeSet,
            usee : DGNode,
            finish : () => Unit) = new ElementConstraintEditor(constraint, users, usee, finish)
}

class ElementConstraintEditor private (constraint0 : Constraint,
                                       users0 : RangeSet,
                                       usee0 : DGNode,
                                       finish0 : () => Unit)
  extends ConstraintEditor(constraint0, users0, usee0, finish0){

  def onReload() : Unit = {
    val set = LiteralRangeSet()
    ???
    /*users.foreach{ u =>
      if(constraint.isViolatedBy(AGEdge.uses(u, usee))){
        set += u
      }
    }*/
    users = set
  }

  def init() : Unit = {
    val contents = mutable.Buffer[Component]()
    //Owners
    contents += new OwnerPanel(this)

    contents += ConstraintEditor.separator()

    //Interlopers
    contents += new InterlopersPanel(this)

    contents += ConstraintEditor.separator()

    contents += new FriendPanel(this)
    ()
    //init(contents.iterator)
  }
}



*/
