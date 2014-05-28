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
    case Class() | Constructor() => "#FFFF33" //Yellow
    case Method() | Field() => "#FFFFFF" //White
    case Literal() => "#CCFFCC" //Very Light green
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
           case Field() => (n::fds, cts, mts, cls)
           case Constructor() => (fds, n::cts, mts, cls)
           case Method() => (fds, cts, n::mts, cls)
           case _ => throw new Error("Wrong NodeKind contained by a class")
         }
      }
  }

  override def apply(g: AccessGraph,
            id: Int, name : String,
            kind : NodeKind) : AGNode = new JavaNode(g, id, name, kind)

  /*
    using the Prolog constraint convention as key eases the node finding when parsing constraints
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

  override def canContain(k : NodeKind) : Boolean = {
    (this.kind, k) match {
      case (Package(), Package())
           | (Package(), Class())
           | (Package(), Interface())
           //| (Class(), Class())
           | (Class(), Constructor())
           | (Class(), Field())
           | (Class(), Method())
           | (Interface(), AbstractMethod())
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

  override def isUserOfItsAbstractionKind = {
    //TODO find if valid !! depend only of the kind ??
    this.kind match{
      case Class() | Interface() => true
      case _ => false
    }
  }

  override def createAbstraction() = {
    // TODO find a strategy or way to make the user choose which abstractkind is used !
    if(kind.abstractKinds.head == Interface()){
      val abs = createNodeAbstraction()
      abs.users_+=(this)
      content.foreach { (child: AGNode) =>
        child.kind match {
          case Method() | AbstractMethod() =>
            abs.content_+=(child.createNodeAbstraction())
          case _ => ()
        }
      }
      abs
    }
    else super.createAbstraction()
  }
}