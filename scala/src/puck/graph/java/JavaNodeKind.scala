package puck.graph.java

import puck.graph._

/**
 * Created by lorilan on 27/05/14.
 */

object JavaNodeKind {
  case class Package private[java]() extends NodeKind {  //unused in LJ
  val abstractKinds = List(`package`)
  }
  case class Interface private[java]() extends NodeKind { //unused in LJ
  val abstractKinds = List(interface)
  }
  case class Class private[java]() extends NodeKind {
    val abstractKinds = List(interface) //  `class` ?
  }
  case class Constructor private[java]() extends NodeKind with HasType[Arrow]{
    def abstractKinds = List(method(`type`))
  }
  case class Method private[java]() extends NodeKind with HasType[Arrow] {
    def abstractKinds = List[NodeKind](abstractMethod(`type`), method(`type`))
  }
  case class Field private[java]() extends NodeKind with HasType[NamedType]{
    //TODO check abstraction : FieldRead != FieldWrite
    // fieldread abstraction type = () -> t
    // fielwrite abstraction type = t -> () (think of t -> t case of jrrt ... )
    def abstractKinds = List(Method())
  }

  case class AbstractMethod private[java]() extends NodeKind with HasType[Arrow] {
    def abstractKinds = List(abstractMethod(`type`), method(`type`))
  }

  case class Literal private[java]() extends NodeKind with HasType[NamedType]{
    //TODO in case of method abstraction cf field comment
    def abstractKinds = List(field(`type`), Method())
  }

  def typedKind[S<:Type, T<:HasType[S]]( ctr : Unit => T, t: S) = {
    val k = ctr()
    k.`type` = t
    k
  }

  val `package` = Package()

  val interface = Interface()
  val `class` = Class()

  def constructor(t : Arrow) = typedKind ( _ => Constructor(), t )
  def method(t : Arrow) = typedKind ( _ => Method(), t )
  def field(t : NamedType) = typedKind ( _ => Field(), t )
  def abstractMethod(t : Arrow) = typedKind ( _ => AbstractMethod(), t )
  def literal(t : NamedType) = typedKind ( _ => Literal(), t )

  /*def constructor(t : Type) = new Constructor(t)
  def method(t : Type) = new Method(t)
  def field(t : Type) = new Field(t)

  def abstractMethod(t : Type) = new AbstractMethod(t)

  def literal(t : Type) = new Literal(t)*/

  //fix for accessing the field in java
  val interfaceKind = interface
  val classKind = `class`
}
