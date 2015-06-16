package puck.graph.io

/**
 * Created by lorilan on 13/08/14.
 */
sealed abstract class DotOutputFormat

case object Png extends DotOutputFormat{
  override def toString = "png"
}

case object Svg extends DotOutputFormat{
  override def toString = "svg"
}