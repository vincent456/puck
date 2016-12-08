package puck.piccolo

import java.awt.Color
import java.awt.geom.Point2D
import java.beans.{PropertyChangeEvent, PropertyChangeListener}

import org.piccolo2d.{PLayer, PNode}
import org.piccolo2d.extras.nodes.PComposite
import puck.control.PuckControl
import puck.graph.NodeIdP
import puck.piccolo.util.{Arrow, Circle, FullTriangle}

import scala.collection.mutable

/**
  * Created by lorilan on 6/3/16.
  */

object PUses {


  def apply(source : DGExpandableNode,
            target : DGExpandableNode,
            edgeLayer : PLayer) : PUses = {
    println("creating " +  (source.id -> target.id))
    val u = new PUses(source, target)
    edgeLayer addChild u
    u
  }

  val property_uses_delete = "usesDelete"

  val property_code_uses_delete = 1 << 11 // PNode default properties goes up to 1 << 10
}

import PUses._
case class PUses(source : DGExpandableNode,
                 target : DGExpandableNode)
  extends PComposite {

  def toNodeIdP = (source.id, target.id)

  def addArrow() : Unit = addArrow(source.arrowGlobalBounds.getCenter2D,
                          target.arrowGlobalBounds.getCenter2D)

  def addArrow(arrowSrc : Point2D, arrowTgt : Point2D) : Unit = Option(getRoot) foreach { root =>
    val control = root.getAttribute("control").asInstanceOf[PuckControl]

    val forbidden = control.constraints exists {
      cm => usesSet exists {
        case (user, used) => cm.isForbidden(control.graph, user, used)
      }
    }

    val numUses = usesSet.size
    val headStyle =
      if(numUses > 1) Circle
      else FullTriangle

   val arrow  = Arrow(arrowSrc, arrowTgt, headStyle)

    if(numUses > 1){
      arrow.addLabel(numUses.toString)
    }

    if(forbidden) {
      arrow setPaint Color.RED
      arrow setStrokePaint Color.RED
    }

    this addChild arrow
  }



  object usesSet extends mutable.HashSet[NodeIdP] {
    override def +=(u : NodeIdP) = {
      if(addElem(u)) {
        PUses.this.removeAllChildren()
        PUses.this.addArrow()
      }

      this
    }

    def ++=(uses : Iterable[NodeIdP]) = {
      var elemAdded = false
      uses foreach {u =>
        elemAdded = elemAdded | addElem(u)
      }
      if(elemAdded) {
        PUses.this.removeAllChildren()
        PUses.this.addArrow()
      }

    }
    override def -=(u : NodeIdP) = {
      if(removeElem(u)) {
        PUses.this.removeAllChildren()
        PUses.this.addArrow()
      }
      this
    }
  }


  {
    val listener = new PropertyChangeListener() {
      def propertyChange(evt: PropertyChangeEvent): Unit = {
        PUses.this.removeAllChildren()
        addArrow()
      }
    }
    for {
      exty <- List(source, target)
      pty <-
      List(PNode.PROPERTY_TRANSFORM,
          PNode.PROPERTY_BOUNDS,
          PNode.PROPERTY_FULL_BOUNDS,
          PNode.PROPERTY_PAINT)
    }
    exty.addPropertyChangeListener(pty,listener)


    for (exty <- List(source, target)){
      exty.addPropertyChangeListener(PNode.PROPERTY_VISIBLE,
        // ie an ancestor is detached, must find first visible ancestor
        new PropertyChangeListener() {
        def propertyChange(evt: PropertyChangeEvent): Unit = delete()
      })

      exty.body.addPropertyChangeListener(PNode.PROPERTY_CHILDREN,
        // new children ! potentially new real extremity to attach
        new PropertyChangeListener() {
          def propertyChange(evt: PropertyChangeEvent): Unit = delete()
        })
    }
  }



  def delete(): Unit = {
    removeFromParent()

    firePropertyChange(property_code_uses_delete, property_uses_delete, null, null)
  }

}
