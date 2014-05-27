package puck.graph.java

import puck.graph.{NodeKind, Type}

/**
 * Created by lorilan on 27/05/14.
 */
object JavaNodeKind {
  case class Package private[JavaNodeKind]() extends NodeKind {  //unused in LJ
  val abstractKinds = List(`package`)
  }
  case class Interface private[JavaNodeKind]() extends NodeKind { //unused in LJ
  val abstractKinds = List(interface)
  }
  case class Class private[JavaNodeKind]() extends NodeKind {
    val abstractKinds = List(interface) //  `class` ?
  }
  case class Constructor private[JavaNodeKind](t : Type) extends NodeKind {
    val abstractKinds = List(method(t))
  }
  case class Method private[JavaNodeKind](t : Type) extends NodeKind {
    val abstractKinds = List[NodeKind](abstractMethod(t), method(t))
  }
  case class Field private[JavaNodeKind](t : Type) extends NodeKind{
    //TODO check abstraction : FieldRead != FieldWrite
    // fieldread abstraction type = () -> t
    // fielwrite abstraction type = t -> () (think of t -> t case of jrrt ... )
    val abstractKinds = List(method(t))
  }

  case class AbstractMethod private[JavaNodeKind](t : Type) extends NodeKind {
    val abstractKinds = List(method(t))
  }

  case class Literal private[JavaNodeKind](t : Type) extends NodeKind {
    //TODO in case of method abstraction cf field comment
    val abstractKinds = List(field(t), method(t))
  }

  val `package` = new Package()

  val interface = new Interface()
  val `class` = new Class()

  def constructor(t : Type) = new Constructor(t)
  def method(t : Type) = new Method(t)
  def field(t : Type) = new Field(t)

  def abstractMethod(t : Type) = new AbstractMethod(t)

  def literal(t : Type) = new Literal(t)

  //fix for accessing the field in java
  val interfaceKind = interface
  val classKind = `class`
}
