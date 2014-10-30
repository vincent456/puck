package puck.graph.mutable.io

/**
 * Created by lorilan on 13/08/14.
 */
sealed abstract class DotOutputFormat
case class Png() extends DotOutputFormat{
  override def toString = "png"
}
case class Svg() extends DotOutputFormat{
  override def toString = "svg"
}