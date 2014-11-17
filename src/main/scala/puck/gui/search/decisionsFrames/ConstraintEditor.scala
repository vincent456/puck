package puck.gui.search.decisionsFrames

import puck.graph.mutable.{AGNode, NodeKind, AGEdge}
import puck.graph.mutable.constraints._
import scala.swing._
import puck.graph.AGError
import java.awt.Color
import scala.Some
import scala.collection.mutable


/**
 * Created by lorilan on 10/06/14.
 */

object ConstraintEditor{
  def separator() =
    new BoxPanel(Orientation.Vertical) {
      contents += new Label(", ")
      contents += Swing.VGlue
    }
}

abstract class ConstraintEditor[K <: NodeKind[K], T<: Constraint[K]] protected (val constraint : T,
                                                           var users : NodeSet[K],
                                                           val usee : AGNode[K],
                                                           finish : () => Unit)
  extends BoxPanel(Orientation.Vertical){

  def init()
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

  init()

}


class FriendPanel[K <: NodeKind[K], T <: ConstraintWithInterlopers[K]](val editor : ConstraintEditor[K, T])
  extends BoxPanel(Orientation.Vertical){

  val constraint : T = editor.constraint
  val users : NodeSet[K] = editor.users
  val usee : AGNode[K] = editor.usee

  val nsd: NodeSetDiff[K] = new NodeSetDiff(users, LiteralNodeSet())
  constraint.friends.foreach { f =>
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


  }

  if (contents.isEmpty)
    contents += new Label("[]")

  contents+= Swing.VGlue

}

class InterlopersPanel[K <: NodeKind[K], T <: ConstraintWithInterlopers[K]](val editor : ConstraintEditor[K, T])
  extends BoxPanel(Orientation.Vertical){
  val constraint : T = editor.constraint
  val users : NodeSet[K] = editor.users
  val usee : AGNode[K] = editor.usee

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
  }
  contents+= Swing.VGlue
}

class OwnerPanel[K <: NodeKind[K], T <: Constraint[K]](val editor : ConstraintEditor[K, T])
  extends BoxPanel(Orientation.Vertical){

  val constraint : T = editor.constraint
  val users : NodeSet[K] = editor.users
  val usee : AGNode[K] = editor.usee

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
  }
  contents+= Swing.VGlue
}

object ScopeConstraintEditor{
  def apply[K <: NodeKind[K]](constraint : ScopeConstraint[K],
            users : NodeSet[K],
            usee : AGNode[K],
            finish : () => Unit) = new ScopeConstraintEditor(constraint, users, usee, finish)
}

class ScopeConstraintEditor[K <: NodeKind[K]] private (constraint0 : ScopeConstraint[K],
                                     users0 : NodeSet[K],
                                     usee0 : AGNode[K],
                                     finish0 : () => Unit)
  extends ConstraintEditor[K, ScopeConstraint[K]](constraint0, users0, usee0, finish0){

  def onReload(){
    val set = LiteralNodeSet[K]()
    users.foreach{ u =>
      if(constraint.isViolatedBy(AGEdge.uses(u, usee))){
        set += u
      }
    }
    users = set
  }

  def init(){
    val contents = mutable.Buffer[Component]()

    contents += new OwnerPanel(this)

    contents += ConstraintEditor.separator()

    /**
     * Facade
     */
    contents += new BoxPanel(Orientation.Vertical){

      var needNewCell = true
      constraint.facades.foreach { f =>
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
    init(contents.iterator)
  }

}


object ElementConstraintEditor{
  def apply[K <: NodeKind[K]](constraint : ElementConstraint[K],
            users : NodeSet[K],
            usee : AGNode[K],
            finish : () => Unit) = new ElementConstraintEditor(constraint, users, usee, finish)
}

class ElementConstraintEditor[K <: NodeKind[K]] private (constraint0 : ElementConstraint[K],
                                       users0 : NodeSet[K],
                                       usee0 : AGNode[K],
                                       finish0 : () => Unit)
  extends ConstraintEditor[K, ElementConstraint[K]](constraint0, users0, usee0, finish0){

  def onReload(){
    val set = LiteralNodeSet[K]()
    users.foreach{ u =>
      if(constraint.isViolatedBy(AGEdge.uses(u, usee))){
        set += u
      }
    }
    users = set
  }

  def init(){
    val contents = mutable.Buffer[Component]()
    //Owners
    contents += new OwnerPanel(this)

    contents += ConstraintEditor.separator()

    //Interlopers
    contents += new InterlopersPanel(this)

    contents += ConstraintEditor.separator()

    contents += new FriendPanel(this)
    init(contents.iterator)
  }





}
