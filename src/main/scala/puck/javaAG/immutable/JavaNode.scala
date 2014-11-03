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

object JavaNode extends AGNodeBuilder[JavaNodeKind, DeclHolder] with DotHelper[JavaNodeKind]{
  def apply(graph : AccessGraph[JavaNodeKind, DeclHolder],
            id : NodeId[JavaNodeKind],
            name : String,
            kind : JavaNodeKind,
            styp : TypeHolder[JavaNodeKind],
            isMutable : Mutability,
            t : DeclHolder) : JavaNode =
    new JavaNode(graph, id, name, kind, styp, isMutable, t)

  def createT() = EmptyDeclHolder

  def rootKind : JavaNodeKind = JavaRoot
  def kinds : Seq[JavaNodeKind] = JavaNodeKind.list

  override def isDotSubgraph(k: JavaNodeKind): Boolean = k == Package

  override def namePrefix(k: JavaNodeKind): String =  k match {
    case Package => "&lt;&lt;package&gt;&gt; "
    case Interface => "&lt;&lt;interface&gt;&gt; "
    case _ => ""
  }

  override def splitDotClassContent(graph : AccessGraph[JavaNodeKind, _], n: NodeId[JavaNodeKind]) = {
    graph.getNode(n).content.foldLeft( (Seq[NodeId[JavaNodeKind]](), Seq[NodeId[JavaNodeKind]](), Seq[NodeId[JavaNodeKind]](), Seq[NodeId[JavaNodeKind]]()) ){
      ( lists : (Seq[NodeId[JavaNodeKind]], Seq[NodeId[JavaNodeKind]], Seq[NodeId[JavaNodeKind]] , Seq[NodeId[JavaNodeKind]]), n : NodeId[JavaNodeKind] ) =>
        val (fds, cts, mts, cls) = lists
        val kind = graph.getNode(n).kind
        kind match {
          case Interface | Class => (fds, cts, mts, n+:cls)
          case Field => (n+:fds, cts, mts, cls)
          case Constructor => (fds, n+:cts, mts, cls)
          case AbstractMethod
               | Method => (fds, cts, n+:mts, cls)

          case _ => throw new Error(kind + " : wrong NodeKind contained by a class" )
        }
    }
  }

  override def isDotClass(k: JavaNodeKind): Boolean = k match { case Class | Interface => true; case _ => false}

  override def fillColor(k: JavaNodeKind): String = k match {
    case Package => "#FF9933" //Orange
    case Interface => "#FFFF99" // Light yellow
    case Class | Constructor => "#FFFF33" //Yellow
    case Method | Field => "#FFFFFF" //White
    case Literal => "#CCFFCC" //Very Light green
    case _ => throw new Error("Unknown JavaNodeKind")
  }
}

class JavaNode
( graph : AccessGraph[JavaNodeKind, DeclHolder],
  id : NodeId[JavaNodeKind],
  name : String,
  override val kind : JavaNodeKind,
  styp : TypeHolder[JavaNodeKind],
  isMutable : Mutability,
  t : DeclHolder)
  extends AGNode[JavaNodeKind, DeclHolder](graph, id, name, kind, styp, isMutable, t){

  override type NodeIdT = NodeId[JavaNodeKind]

  override def canContain(otherId : NodeIdT) : Boolean = {
    val n = graph.getNode(otherId)
    def noNameClash( l : Int )( cId : NodeIdT ) : Boolean = {
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

  //A merging candidate is either structurally equal
  //either a subtype of this
  //hence if we do the merge "this" will disappear
  // and all its user redirected to the candidate
  /*override def findMergingCandidate() : Option[AGNode[JavaNodeKind]] = this.kind match{

    case k @ Interface() if this.content.nonEmpty =>
      graph.find{ otherItc =>
        otherItc.kind match {
          case otherk @ Interface() if otherItc != this =>
            (k isMergingCandidate otherk) &&
              this.users.forall(!_.interloperOf(otherItc)) &&
              this.uses.forall(!otherItc.interloperOf(_))

          case _ => false
        }
      }
    case _ => None
  }*/


}
