package puck.javaAG

import puck.graph._
import JavaNodeKind._
import puck.graph.constraints.{AbstractionPolicy, Move, SupertypeAbstraction, RedirectionPolicy}

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

  def isSubtypeOf(other : AGNode) = {
    (this.kind, other.kind) match {
      case (Class(), Interface()) => new JavaType(this).subtypeOf(new JavaType(other))
      case (Class(), Class()) => new JavaType(this).subtypeOf(new JavaType(other))
      case _ => false
    }
  }

  override def searchExistingAbstractions(){
    println("searching abstractions for " +this)
    graph.iterator foreach { n =>
      if(n != this && (this isSubtypeOf n) &&
        //to be a valid abstraction, in addition to be a supertype,
        // needed method signature
        users.forall {
          user =>
            user.sideUses(this) match {
              case Some(sideUses) =>
                sideUses.forall{ sideUse =>
                n.content.exists(nchild =>
                  new JavaType(nchild).subtypeOf(new JavaType(sideUse.target)))
              }
              case None => true
            }
        })
        println("found " + n)
        this.abstractions0 += ((n, SupertypeAbstraction()))
    }
    println("search terminated")
  }

  override def createAbstraction(abskind : NodeKind,
                                 policy : AbstractionPolicy) = {
    (abskind, policy) match {
      case (Interface(), SupertypeAbstraction()) =>
        val abs = createNodeAbstraction(Interface(), SupertypeAbstraction())
        abs.users_+=(this)
        content.foreach { (child: AGNode) =>
          child.kind match {
            case Method() | AbstractMethod() =>
              abs.content_+=(child.createNodeAbstraction(AbstractMethod(),
                SupertypeAbstraction()))
            case _ => ()
          }
        }
        this.superTypes_+=(abs)
        abs

      //no (abs, impl) or (impl, abs) uses
      //case (AbstractMethod(), SupertypeAbstraction()) =>
        /*val cterAbs =
          container_!.createAbstraction(Interface(), SupertypeAbstraction())
        this.abstractions.find{
          case (n , SupertypeAbstraction()) =>
             n.container_! == container_!
          case _ => false
        } match {
          case Some(abs) => abs
          case None => throw new AGError("Error while creating abstract method !")
        }
      */
      case _ => super.createAbstraction(abskind, policy)
    }
  }

  override def moveTo(newContainer : AGNode) {
//    println("moving " + this +" from " + container_! + " to " + newContainer)
    val oldContainer = container_!
    this.kind match{
      case Method()
           // | AbstractMethod() // ?
           | Field() =>
        this.users.foreach{ user =>
          /* We create a use toward the container that will be redirected
           * when redirecting primary uses
           */
          if(user.container == this.container){
            graph.addUsesDependency(user, user.container_!,
              user, this)
//            println("addding uses dependancy ("+ user + ", " + user.container_!
//              + ") (" + user +", " + this + ")")
          }
          if(container_!.uses(this)){
            graph.addUsesDependency(oldContainer, oldContainer,
              oldContainer, this)
//            println("addding uses dependancy ("+ oldContainer + ", " + oldContainer
//              + ") (" + oldContainer +", " + this + ")")
          }
        }
      case _ => ()
    }


    super.moveTo(newContainer)
  }


}