package puck.javaAG

import puck.graph._
import JavaNodeKind._
import puck.graph.constraints.AbstractionPolicy

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
           case AbstractMethod()
             | Method() => (fds, cts, n::mts, cls)

           case _ => throw new Error(n.kind + " : wrong NodeKind contained by a class" )
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

  val kinds : List[NodeKind] = JavaNodeKind.list
}

class JavaNode( graph : AccessGraph,
                id : Int,
                name : String,
                kind : NodeKind)
  extends AGNode(graph, id, name, kind){

  override def `may be an abstraction of`(other : AGNode) = {
    (this.kind, other.kind) match {
      case (Interface(), Class()) => new JavaType(other).subtypeOf(new JavaType(this))
      case (Class(), Class()) => new JavaType(other).subtypeOf(new JavaType(this))
      case _ => false
    }

  }

  override def createAbstraction(abskind : NodeKind,
                                 policy : AbstractionPolicy) = {
    abskind match {
      case Interface() =>
        val abs = createNodeAbstraction(abskind)
        abs.users_+=(this)
        content.foreach { (child: AGNode) =>
          child.kind match {
            case Method() | AbstractMethod() =>
              abs.content_+=(child.createNodeAbstraction(AbstractMethod()))
            case _ => ()
          }
        }
        abs
      case _ => super.createAbstraction(abskind, policy)
    }
  }

}