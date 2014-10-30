package puck.javaAG.immutable

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable._
import puck.javaAG.JavaAGError
import puck.javaAG.immutable.nodeKind._


/**
 * Created by lorilan on 27/05/14.
 */


class JavaNamedType(n : NodeId[JavaNodeKind], name : String) extends NamedType[JavaNodeKind](n, name){

 override def create(n : NodeId[JavaNodeKind], name : String) = new JavaNamedType(n, name)

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

  def createReturnAccess(graph : AccessGraph[JavaNodeKind]) =
    graph.getNode(output.node).kind match {
    case tk : TypeKind => tk.createLockedAccess()
    case _ => throw new JavaAGError("need a typekind as output node")
  }

  def createASTParamList(graph : AccessGraph[JavaNodeKind]) : Seq[AST.ParameterDeclaration] = {
    input.types.map { t =>
      val node = graph.getNode(t.node)
      node.kind match {
        case tk: TypeKind =>
          new AST.ParameterDeclaration(new AST.Modifiers,
            tk.createLockedAccess(),
            node.name.toLowerCase)
        case _ => throw new JavaAGError("need type kind for param list !")
      }

    }
  }
}

case class Predefined(pkg: String,  name : String, kind : JavaNodeKind){
  def fullName = pkg + "." + name
}

object Predefined {

  val void = Predefined("@primitive", "void", Primitive(-1, None))
  val boolean = Predefined("@primitive", "boolean", Primitive(-2, None))
  val byte = Predefined("@primitive", "byte", Primitive(-3, None))
  val char = Predefined("@primitive", "char", Primitive(-4, None))
  val double = Predefined("@primitive", "double", Primitive(-5, None))
  val float = Predefined("@primitive", "float", Primitive(-6, None))
  val int = Predefined("@primitive", "int", Primitive(-7, None))
  val long = Predefined("@primitive", "long", Primitive(-8, None))
  val short = Predefined("@primitive", "short", Primitive(-9, None))

  val string = Predefined("java.lang", "String", JavaNodeKind.classKind.create(-10)) // not a primitive ...

  val stringLiteralPrototype = Literal(AccessGraph.dummyId, new JavaNamedType(-10, "string"))

  val list = List(void, boolean, byte, char, double, float, int, long, short, string)

}