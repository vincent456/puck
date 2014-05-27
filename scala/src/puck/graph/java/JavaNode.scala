package puck.graph.java

import puck.graph._
import JavaNodeKind._
/**
 * Created by lorilan on 06/05/14.
 */


object JavaNode extends DotHelper with AGNodeBuilder{
  def isDotSubgraph(k:NodeKind) = k match {case Package() => true; case _ => false}
  def isDotClass(k:NodeKind)= k match { case Class() | Interface() => true; case _ => false}
  def fillColor(k:NodeKind)= k match {
    case Package() => "#FF9933" //Orange
    case Interface() => "#FFFF99" // Light yellow
    case Class() | Constructor(_) => "#FFFF33" //Yellow
    case Method(_) | Field(_) => "#FFFFFF" //White
    case Literal(_) => "#CCFFCC" //Very Light green
    case _ => throw new Error("Unknown JavaNodeKind")
  }

  def namePrefix(k:NodeKind)= k match {
    case Package() => "&lt;&lt;package&gt;&gt; "
    case Interface() => "&lt;&lt;interface&gt;&gt; "
    case _ => ""
  }

  def splitDotClassContent(n: AGNode)={
      n.content.foldLeft( (List[AGNode](), List[AGNode](), List[AGNode](), List[AGNode]()) ){
        ( lists : (List[AGNode], List[AGNode], List[AGNode] , List[AGNode]), n : AGNode ) =>
         val (fds, cts, mts, cls) = lists
         n.kind match{
           case Interface() | Class() => (fds, cts, mts, n::cls)
           case Field(_) => (n::fds, cts, mts, cls)
           case Constructor(_) => (fds, n::cts, mts, cls)
           case Method(_) => (fds, cts, n::mts, cls)
           case _ => throw new Error("Wrong NodeKind contained by a class")
         }
      }
  }

  override def apply(g: AccessGraph,
            id: Int, name : String,
            kind : NodeKind) : AGNode = AGNode(g,id,name,kind)

  /*
    using the Prolog constraint convention as key ease the node finding when parsing constraints
   */
  override def makeKey(fullName: String, localName:String,
                       kind: NodeKind) :String =
    AGNode.makeKey(fullName, localName, kind)
  /*
  override def makeKey(fullName: String, localName:String, kind: NodeKind, `type`: Option[Type]) : String = {
    (kind, `type`) match {
      case (_, None) => fullName
      case (Field(), _) => fullName
      case (Constructor(), Some(t)) => fullName + "#_" + prologTypeString(t)
      case (Method(), Some(t)) =>  fullName + "__" + prologTypeString(t)
      case _ => throw new Error("don't know how to do a key - should not happen")
    }
  }

  private def prologTypeString(t: Type) : String = t match {
    case NamedType(name, id) => (name split "[.]").last
    case Tuple(tu) => tu mkString "_"
    case Arrow(i, _) => prologTypeString(i)
  }
   */
}

class JavaNode( graph : AccessGraph,
                id : Int,
                name : String,
                kind : NodeKind)
  extends AGNode(graph, id, name, kind){

  def canContain(other : AGNode) : Boolean = {
    (this.kind, other.kind) match {
      case (Package(), Package())
           | (Package(), Class())
           | (Package(), Interface())
           //| (Class(), Class())
           | (Class(), Constructor(_))
           | (Class(), Field(_))
           | (Class(), Method(_))
           | (Interface(), AbstractMethod(_))
      => true

      case _ => false
    }
  }

  override def `may be an abstraction of`(other : AGNode) = {
    (this.kind, other.kind) match {
      case (Interface(), Class()) => new JavaType(other).subtypeOf(new JavaType(this))
      case (Class(), Class()) => new JavaType(other).subtypeOf(new JavaType(this))
      case _ => false
    }

  }
}