package puck.graph

object UsesAccessKind {

  //java accessors
  val none : Option[UsesAccessKind] = None
  val read : Option[UsesAccessKind] = Some(Read)
  val write : Option[UsesAccessKind] = Some(Write)
  val rw : Option[UsesAccessKind] = Some(RW)
}
sealed abstract class UsesAccessKind {
  def && (accK : UsesAccessKind) : UsesAccessKind
}
case object Read extends UsesAccessKind {
  def && (accK : UsesAccessKind) : UsesAccessKind = accK match {
    case Read => Read
    case _ => RW
  }
}
case object Write extends UsesAccessKind {
  def && (accK : UsesAccessKind) : UsesAccessKind = accK match {
    case Write => Write
    case _ => RW
  }
}
case object RW extends UsesAccessKind{
  def && (accK : UsesAccessKind) : UsesAccessKind = this
}