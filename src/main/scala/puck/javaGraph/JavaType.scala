package puck.javaGraph

import puck.graph._

/**
 * Created by lorilan on 27/05/14.
 */


class JavaNamedType(n : NodeId) extends NamedType(n){

 override def create(n : NodeId) = new JavaNamedType(n)

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
  type InputType =  Tuple[NamedType]
  type OutputType = NamedType

  type T =  Arrow[Tuple[NamedType], NamedType]

/*  def unapply( m : MethodType) : Option[(MethodType.InputType, MethodType.OutputType)] =
  Some(m.input, m.output)*/
}

class MethodType(i: MethodType.InputType,
                 o: MethodType.OutputType)
  extends Arrow[Tuple[NamedType], NamedType](i, o){

  /*override def equals(other : Any) = other match {
    case that : MethodType => that.canEqual(this) &&
      that.input == this.input && that.output == this.output
    case _ => false
  }

  def canEqual( that : MethodType ) = true

  override def hashCode = 41 * input.hashCode + output.hashCode() + 41*/
  override def toString = "MethodType(" + input +" -> " + output +")"

  override def canOverride(other : Type[_]) : Boolean =
    other match {
      case om : MethodType => om.input == input &&
        output.subtypeOf(om.output)
      case _ => false
    }


  override def create(i : MethodType.InputType,
              o : MethodType.OutputType) = new MethodType(i, o)

  def createReturnAccess(graph : DependencyGraph,
                         id2Decl : Map[NodeId, ASTNodeLink]) =
    id2Decl(output.id) match {
    case tk : TypedKindDeclHolder => tk.decl.createLockedAccess()
    case _ => throw new JavaAGError("need a typekind as output node")
  }

  def createASTParamList(graph : DependencyGraph,
                         id2Decl : Map[NodeId, ASTNodeLink]) : Seq[AST.ParameterDeclaration] = {
    input.types.map { ty =>
      val node = graph.getConcreteNode(ty.id)
      id2Decl(ty.id) match {
        case tk : TypedKindDeclHolder =>

          new AST.ParameterDeclaration(new AST.Modifiers, tk.decl.createLockedAccess(), node.name.toLowerCase)

        case _ => throw new JavaAGError("need type kind for param list !")
      }

    }
  }
}

case class Predefined(id : NodeId, pkg: String,  name : String, kind : JavaNodeKind){
  def fullName = pkg + "." + name
}

object Predefined {

  /*val void = Predefined(-1, "@primitive", "void", Primitive)
  val boolean = Predefined(-2, "@primitive", "boolean", Primitive)
  val byte = Predefined(-3, "@primitive", "byte", Primitive)
  val char = Predefined(-4, "@primitive", "char", Primitive)
  val double = Predefined(-5, "@primitive", "double", Primitive)
  val float = Predefined(-6, "@primitive", "float", Primitive)
  val int = Predefined(-7, "@primitive", "int", Primitive)
  val long = Predefined(-8, "@primitive", "long", Primitive)
  val short = Predefined(-9, "@primitive", "short", Primitive)

  val string = Predefined(-10, "java.lang", "String", Class) // not a primitive ...
  val stringTyp = NamedTypeHolder(new JavaNamedType(-10, "string"))

  val list = List(void, boolean, byte, char, double, float, int, long, short, string)*/

}
