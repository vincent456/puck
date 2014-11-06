package puck.javaAG.immutable

import puck.graph.AGError
import puck.graph.immutable.AccessGraph.{NodeId, Mutability}
import puck.graph.io.DotHelper
import puck.graph.immutable._
import puck.javaAG.immutable.nodeKind._

/**
 * Created by lorilan on 29/10/14.
 */

import scala.language.existentials

object JavaNode extends AGNodeBuilder with DotHelper{
  def apply(graph : AccessGraph,
            id : NodeId,
            name : String,
            kind : NodeKind,
            styp : TypeHolder,
            isMutable : Mutability,
            t : Hook) : JavaNode =
    new JavaNode(graph, id, name, kind, styp, isMutable, t)

  def createT() = EmptyDeclHolder

  def rootKind : JavaNodeKind = JavaRoot
  def kinds : Seq[NodeKind] = JavaNodeKind.list

  override def isDotSubgraph(k: NodeKind): Boolean = k == Package

  override def namePrefix(k: NodeKind): String =  k match {
    case Package => "&lt;&lt;package&gt;&gt; "
    case Interface => "&lt;&lt;interface&gt;&gt; "
    case _ => ""
  }

  override def splitDotClassContent(graph : AccessGraph, n: NodeId) = {
    graph.getNode(n).content.foldLeft( (Seq[NodeId](), Seq[NodeId](), Seq[NodeId](), Seq[NodeId]()) ){
      ( lists : (Seq[NodeId], Seq[NodeId], Seq[NodeId] , Seq[NodeId]), n : NodeId ) =>
        val (fds, cts, mts, cls) = lists
        val kind = graph.getNode(n).kind
        kind match {
          case Interface | Class => (fds, cts, mts, n+:cls)
          case Field => (n+:fds, cts, mts, cls)
          case Constructor => (fds, n+:cts, mts, cls)
          case AbstractMethod
               | Method
               | ConstructorMethod => (fds, cts, n+:mts, cls)

          case _ => throw new Error(kind + " : wrong NodeKind contained by a class" )
        }
    }
  }

  override def isDotClass(k: NodeKind): Boolean = k match { case Class | Interface => true; case _ => false}

  override def fillColor(k: NodeKind): String = k match {
    case Package => "#FF9933" //Orange
    case Interface => "#FFFF99" // Light yellow
    case Class | Constructor => "#FFFF33" //Yellow
    case Method | Field => "#FFFFFF" //White
    case Literal => "#CCFFCC" //Very Light green
    case _ => throw new Error("Unknown JavaNodeKind")
  }
}

class JavaNode
( graph : AccessGraph,
  id : NodeId,
  name : String,
  override val kind : NodeKind,
  styp : TypeHolder,
  isMutable : Mutability,
  t : Hook)
  extends AGNode(graph, id, name, kind, styp, isMutable, t){

  override type NIdT = NodeId

  override def canContain(otherId : NIdT) : Boolean = {
    val n = graph.getNode(otherId)
    def noNameClash( l : Int )( cId : NIdT ) : Boolean = {
      val c = graph.getNode(cId)
      (c.kind, c.styp) match {
         case (ck: MethodKind, MethodTypeHolder(typ))=>
          c.name != n.name || typ.input.length != l
         case (ck: MethodKind, _)=> throw new AGError()
        case _ => true
      }
    }

    super.canContain(otherId) &&
      ( (n.kind, n.styp) match {
        case (AbstractMethod, MethodTypeHolder(absTyp)) =>
          /*
            All subtypes must implement the method
           */
          this.content.forall(noNameClash(absTyp.input.length)) &&
            this.directSubTypes.forall { id =>
              graph.content(id).exists { cid =>
                val c = graph.getNode(cid)
                (c.kind, c.styp) match {
                  case (Method, MethodTypeHolder(typ)) => n.name == c.name && absTyp == typ
                  case (Method, _) => throw new AGError()
                  case _ => false
                }
              }
            }
        case (AbstractMethod, _) => throw new AGError(n + " does not have a MethodTypeHolder")
        /* cannot have two methods with same name and same type */
        case (Method, MethodTypeHolder(typ)) =>
          this.content.forall(noNameClash(typ.input.length))
        case (Method, _) => throw new AGError()
        case _ => true
      })
  }
}
