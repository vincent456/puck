package puck.javaAG

import puck.graph._
import puck.graph.constraints.{Supertype, AbstractionPolicy, Delegation}

/**
 * Created by lorilan on 27/05/14.
 */

object JavaNodeKind {

  case class Package() extends NodeKind {  //unused in LJ

    def canContain(k : NodeKind) : Boolean = {
      k match {
        case Package()
             | Class()
             | Interface() => true
        case _ => false
      }
    }

    override def abstractionPolicies = List(Delegation())
    def abstractKinds(p : AbstractionPolicy) = List(`package`)

    override def canBeRootContent = true
  }

  case class Interface private[javaAG]() extends NodeKind { //unused in LJ

    def canContain(k : NodeKind) : Boolean = {
      k match {
        case AbstractMethod() => true
        case _ => false
      }
    }

    def abstractKinds(p : AbstractionPolicy) = p match {
      case Supertype() => List(interface)
      case Delegation() => List(`class`)//also interface ?
    }
  }
  case class Class private[javaAG]() extends NodeKind {
    def canContain(k : NodeKind) : Boolean = {
      k match {
        case Constructor()
             | Field()
             | Method() => true
        case _ => false
      }
    }

    def abstractKinds(p : AbstractionPolicy) = p match {
      case Supertype() => List(interface, `class`)
      case Delegation() => List(`class`)//also interface ?
    }
  }

  case class Constructor private[javaAG]() extends NodeKind with HasType[Arrow]{
    def canContain(k : NodeKind) = false

    override def abstractionPolicies = List(Delegation())

    def abstractKinds(p : AbstractionPolicy) = List(method(`type`))
  }
  case class Method private[javaAG]() extends NodeKind with HasType[Arrow] {
    def canContain(k : NodeKind) = false

    def abstractKinds(p : AbstractionPolicy) = p match {
      case Supertype() => List(abstractMethod(`type`), method(`type`))
      case Delegation() => List(method(`type`))//also abstractMethod ?
    }
  }

  case class Field private[javaAG]() extends NodeKind with HasType[NamedType]{
    def canContain(k : NodeKind) = false
    //TODO check abstraction : FieldRead != FieldWrite
    // fieldread abstraction type = () -> t
    // fielwrite abstraction type = t -> () (think of t -> t case of jrrt ... )
    def abstractKinds(p : AbstractionPolicy) = List(Method())

    override def abstractionPolicies = List(Delegation())
  }

  case class AbstractMethod private[javaAG]() extends NodeKind with HasType[Arrow] {
    def canContain(k : NodeKind) = false

    def abstractKinds(p : AbstractionPolicy) = p match {
      case Supertype() => List(abstractMethod(`type`))
      case Delegation() => List(method(`type`))//also abstractMethod ?
    }

  }

  case class Literal private[javaAG]() extends NodeKind with HasType[NamedType]{
    def canContain(k : NodeKind) = false
    //TODO in case of method abstraction cf field comment
    override def abstractionPolicies = List(Delegation())
    def abstractKinds(p : AbstractionPolicy) = List(field(`type`), Method())
  }

  def typedKind[S<:Type, T<:HasType[S]]( ctr : () => T, t: S) = {
    val k = ctr ()
    k.`type` = t
    k
  }

  val `package` = Package()

  val interface = Interface()
  val `class` = Class()

  def constructor(t : Arrow) = typedKind ( () => Constructor(), t )
  def method(t : Arrow) = typedKind ( () => Method(), t )
  def field(t : NamedType) = typedKind ( () => Field(), t )
  def abstractMethod(t : Arrow) = typedKind ( () => AbstractMethod(), t )
  def literal(t : NamedType) = typedKind ( () => Literal(), t )

  /*def constructor(t : Type) = new Constructor(t)
  def method(t : Type) = new Method(t)
  def field(t : Type) = new Field(t)

  def abstractMethod(t : Type) = new AbstractMethod(t)

  def literal(t : Type) = new Literal(t)*/

  //fix for accessing the field in java
  val interfaceKind = interface
  val classKind = `class`

  val list = List[NodeKind](Package(), Interface(),
    Class(), Constructor(),
    Method(), Field(), AbstractMethod(), Literal(), Primitive())

}
