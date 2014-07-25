package puck.javaAG

import puck.graph._
import puck.graph.constraints.{AbstractionPolicy, SupertypeAbstraction, DelegationAbstraction}
import puck.javaAG.JavaNodeKind._

/**
 * Created by lorilan on 27/05/14.
 */

abstract class JavaNodeKind extends NodeKind[JavaNodeKind]{
  def createDecl(n : AGNode[JavaNodeKind]) {
    throw new Error("do not know how to create declaration for" + getClass)
  }
}
case class JavaRoot() extends JavaNodeKind with AGRoot[JavaNodeKind]{
  def create() = JavaRoot()
  var program : AST.Program = _
}

object JavaNodeKind {

  def typedKind[S<:Type, T<:HasType[S]]( ctr : () => T, t: S) = {
    val k = ctr ()
    k.`type` = t
    k
  }

  def `package` = Package()

  def interface = Interface()
  def `class` = Class()
  //fix for accessing the field in java
  def interfaceKind = interface
  def classKind = `class`

  def constructor(t : MethodType) = typedKind ( () => Constructor(), t )
  def method(t : MethodType) = typedKind ( () => Method(), t )
  def field(t : JavaType) = typedKind ( () => Field(), t )
  def abstractMethod(t : MethodType) = typedKind ( () => AbstractMethod(), t )
  def literal(t : JavaType) = typedKind ( () => Literal(), t )


  val list = List[JavaNodeKind](Package(), Interface(),
    Class(), Constructor(),
    Method(), Field(), AbstractMethod(), Literal(), Primitive())

  def addDeclToProgram(n : AGNode[JavaNodeKind], decl : AST.TypeDecl){
    val prog = n.graph.root.kind match {
      case r @ JavaRoot() => r.program
      case r => throw new Error("root shoold be of kind JavaRoot instead of " + r)
    }

    decl.setModifiers(new AST.Modifiers("public"))
    val cu = new AST.CompilationUnit()
    cu.setTypeDecl(decl, 0)
    cu.setFromSource(true)
    prog.addCompilationUnit(cu)
  }

}

case class Package() extends JavaNodeKind {

  def create() = Package()
  override def createDecl(n : AGNode[JavaNodeKind]){}
  //var decl : AST.PackageDecl = _

  def canContain(k : JavaNodeKind) : Boolean = {
    k match {
      case Package()
           | Class()
           | Interface() => true
      case _ => false
    }
  }

  override def abstractionPolicies = List(DelegationAbstraction())
  def abstractKinds(p : AbstractionPolicy) = List(`package`)

  override val canBeRootContent = true
}

abstract class TypeKind extends JavaNodeKind {
  def decl : AST.TypeDecl
  def createLockedAccess() : AST.Access = decl.createLockedAccess()
}

case class Interface private[javaAG]() extends TypeKind { //unused in LJ

  def create() = Interface()

  override def createDecl( n : AGNode[JavaNodeKind]){
    assert(n.kind eq this)
    if(decl == null){
      decl  = new AST.InterfaceDecl()
      decl.setID(n.name)
      addDeclToProgram(n, decl)
    }
  }

  var decl : AST.InterfaceDecl = _

  def canContain(k : JavaNodeKind) : Boolean = {
    k match {
      case AbstractMethod() => true
      case _ => false
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(interface)
    case DelegationAbstraction() => List(`class`)//also interface ?
  }
}
case class Class private[javaAG]() extends TypeKind {

  def create() = Class()

  var decl : AST.ClassDecl = _

  override def createDecl( n : AGNode[JavaNodeKind]){
    assert(n.kind eq this)
    if(decl == null){
      decl = new AST.ClassDecl()
      decl.setID(n.name)
      addDeclToProgram(n, decl)
    }
  }

  def canContain(k : JavaNodeKind) : Boolean = {
    k match {
      case Constructor()
           | Field()
           | Method() => true
      case _ => false
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(interface, `class`)
    case DelegationAbstraction() => List(`class`)//also interface ?
  }
}

case class Constructor private[javaAG]() extends JavaNodeKind with HasType[MethodType]{

  def create() = constructor(`type`)

  var decl : AST.ConstructorDecl = _

  def canContain(k : JavaNodeKind) = false

  override def abstractionPolicies = List(DelegationAbstraction())

  def abstractKinds(p : AbstractionPolicy) = p match {
    case DelegationAbstraction() => List( typedKind( () => new ConstructorMethod(), `type`))
    case SupertypeAbstraction() => throw new AGError("Constructor cannot be abstracted by SuperType strategy")
  }

}

case class Field private[javaAG]() extends JavaNodeKind with HasType[JavaType]{

  def create() = field(`type`)

  var decl : AST.FieldDeclaration = _

  def canContain(k : JavaNodeKind) = false
  //TODO check abstraction : FieldRead != FieldWrite
  // fieldread abstraction type = () -> t
  // fielwrite abstraction type = t -> () (think of t -> t case of jrrt ... )
  def abstractKinds(p : AbstractionPolicy) = List(Method())

  override def abstractionPolicies = List(DelegationAbstraction())
}


abstract class MethodKind extends JavaNodeKind with HasType[MethodType] {

  var decl : AST.MethodDecl = _

  def canContain(k : JavaNodeKind) = false

}

case class Method private[javaAG]() extends MethodKind {

  def create() = method(`type`)

  /*override def createDecl(n : AGNode[JavaNodeKind]) = {
    assert(n.kind eq this)
    n.container.content.find{ n1 =>
        n1.kind == Constructor() &&
        n1.abstractions.exists{
          case (n2 , DelegationAbstraction()) => n2 == n
          case _ => false
        }
    } match {
      case None =>
        super.createDecl(n)
      case Some(c) =>
        decl = c.kind.asInstanceOf[Constructor].decl.createConstructorMethod(n.name)
        decl
    }
  }*/

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(abstractMethod(`type`), method(`type`))
    case DelegationAbstraction() => List(method(`type`))//also abstractMethod ?
  }
}

class ConstructorMethod extends Method {

  var ctorDecl : AST.ConstructorDecl = _

  override def create() = { val nc = new ConstructorMethod()
    nc.`type`= this.`type`
    nc
  }

  override def createDecl(n : AGNode[JavaNodeKind]) = {
    assert(n.kind eq this)
    decl = ctorDecl.createConstructorMethod(n.name)
  }
}

case class AbstractMethod private[javaAG]() extends  MethodKind {

  def create() = abstractMethod(`type`)

  override def createDecl( n : AGNode[JavaNodeKind]) = {
    assert(n.kind eq this)
    if(decl == null){
      decl = AST.MethodDecl.createAbstractMethod(`type`.createReturnAccess(),
        n.name, `type`.createASTParamList().toArray)
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(abstractMethod(`type`))
    case DelegationAbstraction() => List(method(`type`))//also abstractMethod ?
  }

}

case class Literal private[javaAG]() extends JavaNodeKind with HasType[JavaType]{

  def create() = literal(`type`)

  def canContain(k : JavaNodeKind) = false
  //TODO in case of method abstraction cf field comment
  override def abstractionPolicies = List(DelegationAbstraction())
  def abstractKinds(p : AbstractionPolicy) = List(field(`type`), Method())
}

case class Primitive private[javaAG] () extends TypeKind {

  def create() = Primitive()

  var decl : AST.TypeDecl = _

  def canContain(k: JavaNodeKind) = false
  def abstractKinds(p : AbstractionPolicy) =
    throw new AGError("do not know how to abstract primitive kind")
}


case class Predefined(pkg: String,  name : String, kind : JavaNodeKind){
  def fullName = pkg + "." + name
}

object Predefined {

  val void = Predefined("@primitive", "void", Primitive())
  val boolean = Predefined("@primitive", "boolean", Primitive())
  val byte = Predefined("@primitive", "byte", Primitive())
  val char = Predefined("@primitive", "char", Primitive())
  val double = Predefined("@primitive", "double", Primitive())
  val float = Predefined("@primitive", "float", Primitive())
  val int = Predefined("@primitive", "int", Primitive())
  val long = Predefined("@primitive", "long", Primitive())
  val short = Predefined("@primitive", "short", Primitive())

  val string = Predefined("java.lang", "String", Class()) // not a primitive ...

  val list = List(void, boolean, byte, char, double, float, int, long, short, string)

  /*val void = Predefined("@primitive.void", -1)
  val boolean = Predefined("@primitive.boolean", -2)
  val byte = Predefined("@primitive.byte", -3)
  val char = Predefined("@primitive.char", -4)
  val double = Predefined("@primitive.double", -5)
  val float = Predefined("@primitive.float", -6)
  val int = Predefined("@primitive.int", -7)
  val long = Predefined("@primitive.long", -8)
  val short = Predefined("@primitive.short", -9)

  val string = Predefined("java.lang.String", -10) // not a primitive ...

  val primitives = List(void, boolean, byte, char, double, float, int, long, short)
*/
  /*
  private def makePrimitiveNode (name_id : (String, Int), g: JavaAccessGraph) =
    new StatelessAGNode(g, name_id._2, name_id._1, Primitive())


  def voidNode(g : JavaAccessGraph) = makePrimitiveNode(void, g)
  def booleanNode(g : JavaAccessGraph) = makePrimitiveNode(boolean, g)
  def byteNode(g : JavaAccessGraph) = makePrimitiveNode(byte, g)
  def charNode(g : JavaAccessGraph) = makePrimitiveNode(char, g)
  def doubleNode(g : JavaAccessGraph) = makePrimitiveNode(double, g)
  def floatNode(g : JavaAccessGraph) = makePrimitiveNode(float, g)
  def intNode(g : JavaAccessGraph) = makePrimitiveNode(int, g)
  def longNode(g : JavaAccessGraph) = makePrimitiveNode(long, g)
  def shortNode(g : JavaAccessGraph) = makePrimitiveNode(short, g)
  def stringNode(g : JavaAccessGraph) = new StatelessAGNode(g, string._2, string._1, `class`)*/


}