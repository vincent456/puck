package puck.graph

/**
 * Created by lorilan on 05/05/14.
 */

abstract class NodeKind
case class AGRoot private[graph]() extends NodeKind
case class VanillaKind private[graph]() extends NodeKind
