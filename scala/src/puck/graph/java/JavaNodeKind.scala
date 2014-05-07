package puck.graph.java

import puck.graph.NodeKind
/**
 * Created by lorilan on 06/05/14.
 */

object JavaNodeKind {
  case class Package private[JavaNodeKind]() extends NodeKind  //unused in core LJ
  case class Interface private[JavaNodeKind]() extends NodeKind //unused in LJ

  case class Class private[JavaNodeKind]() extends NodeKind
  case class Constructor private[JavaNodeKind]() extends NodeKind
  case class Method private[JavaNodeKind]() extends NodeKind
  case class Field private[JavaNodeKind]() extends NodeKind

  case class Literal private[JavaNodeKind]() extends NodeKind

  val `package` = new Package()

  val interface = new Interface()
  val `class` = new Class()

  val constructor = new Constructor()
  val method = new Method()
  val field = new Field()

  val literal = new Literal()

  //fix for accessing the field in java
  val interfaceKind = interface
  val classKind = `class`
}
