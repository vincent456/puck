package puck.graph.java

import puck.graph.NamedType

/**
 * Created by lorilan on 05/05/14.
 */
object Primitive {

  val void = NamedType("@primitive.void", -1)
  val boolean = NamedType("@primitive.boolean", -2)
  val byte = NamedType("@primitive.byte", -3)
  val char = NamedType("@primitive.char", -4)
  val double = NamedType("@primitive.double", -5)
  val float = NamedType("@primitive.float", -6)
  val int = NamedType("@primitive.int", -7)
  val long = NamedType("@primitive.long", -8)
  val short = NamedType("@primitive.short", -9)

  // not primitive but ...
  val string = NamedType("java.lang.String", -10)
}
