package puck.piccolo.util

import java.awt.{Color, Graphics2D, Paint}

import org.piccolo2d.PNode
import org.piccolo2d.util.{PBounds, PPaintContext}

import puck.piccolo.BoundsOp
/**
  * Created by lorilan on 6/3/16.
  */
// cf group example
trait DecoratorGroup extends PNode  {

  val margin: Int = 10
  private val cachedChildBounds: PBounds = new PBounds
  private var comparisonBounds: PBounds = new PBounds

  override def paint(ppc: PPaintContext) : Unit = {
    val paint: Paint = Color.black
    if (paint != null) {
      val g2: Graphics2D = ppc.getGraphics
      g2.setPaint(paint)
      val bounds: PBounds = getUnionOfChildrenBounds(null)
      bounds.setRect(bounds.getX - margin, bounds.getY - margin, bounds.getWidth + 2 * margin, bounds.getHeight + 2 * margin)

      g2.draw(bounds.rectangle)
      //g2.fill(bounds)

    }
  }

  override def computeFullBounds(dstBounds: PBounds): PBounds = {
    val result: PBounds = getUnionOfChildrenBounds(dstBounds)
    cachedChildBounds.setRect(result)
    result.setRect(result.getX - margin, result.getY - margin, result.getWidth + 2 * margin, result.getHeight + 2 * margin)
    localToParent(result)
     result
  }

  override def validateFullBounds: Boolean = {
    comparisonBounds = getUnionOfChildrenBounds(comparisonBounds)
    if (cachedChildBounds != comparisonBounds)
      setPaintInvalid(true)

    super.validateFullBounds
  }

}
