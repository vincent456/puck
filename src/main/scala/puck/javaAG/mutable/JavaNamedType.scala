package puck.javaAG.mutable

import puck.graph.mutable._
import puck.javaAG.JavaAGError
import puck.javaAG.mutable.nodeKind._


/**
 * Created by lorilan on 27/05/14.
 */


class JavaNamedType(n : AGNode[JavaNodeKind]) extends NamedType(n){

 override def create(n : AGNode[JavaNodeKind]) = new JavaNamedType(n)

 /*def hasMethodThatCanOverride(name : String, sig : MethodType) : Boolean =
    n.content.exists{ (childThis : AGNode[JavaNodeKind]) =>
      childThis.name == name &&
        (childThis.kind match {
          case m @ Method() => m.`type`.canOverride(sig)
          case _ => false
        })
    }*/
/*
  // compute structural subtyping in addition to registered named subtyping
  override def subtypeOf(other : Type) : Boolean = super.subtypeOf(other) ||
    ( other match {
      case NamedType(othern) =>
        (n.kind, othern.kind) match {
          case (Interface(), Interface())
            | (Class(), Interface())
            | (Class(), Class()) =>
            /* TODO Find what to do about fields
               for now let's consider only the methods */
            othern.content forall { (childOther : AGNode) =>
                childOther.kind match {
                case m @ Method() => hasMethodThantCanOverride(childOther.name, m.`type`)
                case am @ AbstractMethod() => hasMethodThantCanOverride(childOther.name, am.`type`)
                case _ => true
              }
            }
          case _ => false
        }
      case _ => false
    })*/
}

object MethodType{
  type InputType =  Tuple[JavaNodeKind, NamedType[JavaNodeKind]]
  type OutputType = NamedType[JavaNodeKind]

  type T =  Arrow[JavaNodeKind, MethodType.InputType, MethodType.OutputType]

/*  def unapply( m : MethodType) : Option[(MethodType.InputType, MethodType.OutputType)] =
  Some(m.input, m.output)*/
}

class MethodType(i: MethodType.InputType,
                 o: MethodType.OutputType)
  extends MethodType.T(i, o){

  /*override def equals(other : Any) = other match {
    case that : MethodType => that.canEqual(this) &&
      that.input == this.input && that.output == this.output
    case _ => false
  }

  def canEqual( that : MethodType ) = true

  override def hashCode = 41 * input.hashCode + output.hashCode() + 41*/
  override def toString = "MethodType(" + input +" -> " + output +")"

  override def canOverride(other : Type[JavaNodeKind, _]) : Boolean =
    other match {
      case om : MethodType => om.input == input &&
        output.subtypeOf(om.output)
      case _ => false
    }


  override def create(i : MethodType.InputType,
              o : MethodType.OutputType) = new MethodType(i, o)

  def createReturnAccess() = output.node.kind match {
    case tk : TypeKind => tk.createLockedAccess()
    case _ => throw new JavaAGError("need a typekind as output node")
  }

  def createASTParamList() = {
    input.types.map { t =>

      t.node.kind match {
        case tk: TypeKind =>
          new AST.ParameterDeclaration(new AST.Modifiers,
            tk.createLockedAccess(),
            t.node.name.toLowerCase)
        case _ => throw new JavaAGError("need type kind for param list !")
      }

    }
  }
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

}