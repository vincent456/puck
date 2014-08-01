package puck.javaAG

import puck.graph._
import puck.javaAG.nodeKind._


/**
 * Created by lorilan on 27/05/14.
 */


class JavaType(n : AGNode[JavaNodeKind]) extends NamedType(n){

 override def copy() = new JavaType(n)

 def hasMethodThatCanOverride(name : String, sig : MethodType) : Boolean =
    n.content.exists{ (childThis : AGNode[JavaNodeKind]) =>
      childThis.name == name &&
        (childThis.kind match {
          case m @ Method() => m.`type`.canOverride(sig)
          case _ => false
        })
    }
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

class MethodType(override val input: Tuple[NamedType[JavaNodeKind]],
                 override val output: NamedType[JavaNodeKind]) extends Arrow(input, output){
     def canOverride(other : MethodType) : Boolean =
        other.input == input && output.subtypeOf(other.output)

  override def copy() = new MethodType(Tuple(input.types.map(_.copy())), output.copy())

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


  class Replacer private[MethodType] (oldUsee : AGNode[JavaNodeKind]) {
    def replacedBy(newUsee: AGNode[JavaNodeKind]) = {
      def replaceOrCopy(t: NamedType[JavaNodeKind]) =
        if (t.node == oldUsee) new JavaType(newUsee)
        else t.copy()

      new MethodType(Tuple(input.types.map(replaceOrCopy)),
        replaceOrCopy(output))
    }

  }

  def copyWith (oldUsee : AGNode[JavaNodeKind]) = new Replacer(oldUsee)

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