package puck.javaAG

import puck.graph._
import puck.graph.constraints.{RedirectionPolicy, AbstractionPolicy, SupertypeAbstraction}
import puck.javaAG.nodeKind._


/**
 * Created by lorilan on 06/05/14.
 */


object JavaNode extends DotHelper[JavaNodeKind] with AGNodeBuilder[JavaNodeKind]{
  def isDotSubgraph(k : JavaNodeKind) = k match {case Package() => true; case _ => false}
  def isDotClass(k : JavaNodeKind)= k match { case Class() | Interface() => true; case _ => false}
  def fillColor(k : JavaNodeKind)= k match {
    case Package() => "#FF9933" //Orange
    case Interface() => "#FFFF99" // Light yellow
    case Class() | Constructor() => "#FFFF33" //Yellow
    case Method() | Field() => "#FFFFFF" //White
    case Literal() => "#CCFFCC" //Very Light green
    case _ => throw new Error("Unknown JavaNodeKind")
  }

  def rootKind = JavaRoot()

  def namePrefix(k : JavaNodeKind)= k match {
    case Package() => "&lt;&lt;package&gt;&gt; "
    case Interface() => "&lt;&lt;interface&gt;&gt; "
    case _ => ""
  }

  def splitDotClassContent(n: AGNode[JavaNodeKind])={
    n.content.foldLeft( (List[AGNode[JavaNodeKind]](), List[AGNode[JavaNodeKind]](), List[AGNode[JavaNodeKind]](), List[AGNode[JavaNodeKind]]()) ){
      ( lists : (List[AGNode[JavaNodeKind]], List[AGNode[JavaNodeKind]], List[AGNode[JavaNodeKind]] , List[AGNode[JavaNodeKind]]), n : AGNode[JavaNodeKind] ) =>
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

  override def apply(g: AccessGraph[JavaNodeKind],
                     id: Int, name : String,
                     kind : JavaNodeKind) : AGNode[JavaNodeKind] = new JavaNode(g, id, name, kind)

  override def makeKey(fullName: String, localName:String,
                       kind: JavaNodeKind) :String = fullName

  val kinds : List[JavaNodeKind] = JavaNodeKind.list
}

class JavaNode( graph : AccessGraph[JavaNodeKind],
                id : Int,
                name : String,
                kind : JavaNodeKind)
  extends AGNode[JavaNodeKind](graph, id, name, kind){

  override def canContain(n : AGNode[JavaNodeKind]) : Boolean = {

    def noNameClash(l : Int)(c: AGNode[JavaNodeKind]) : Boolean = c.kind match {

      case ck @(Method() | AbstractMethod()) =>
        c.name != n.name || {

          if(ck.asInstanceOf[HasType[MethodType]].`type` == null)
            println("type is null")

          ck.asInstanceOf[HasType[MethodType]].`type`.input.length != l}
      case _ => true
    }

    super.canContain(n) &&
      (n.kind match {
        case nk@AbstractMethod() =>
          /*
            All subtypes must implement the method
           */
          this.content.forall(noNameClash(nk.`type`.input.length)) &&
            this.subTypes.forall {
              _.content.exists { c =>
                c.kind match {
                  case ck@Method() => n.name == c.name && nk.`type` == ck.`type`
                  case _ => false
                }
              }
            }
        /*
          cannot have two methods with same name and same type
          */
        case nk @ Method() =>
          this.content.forall(noNameClash(nk.`type`.input.length))

        case _ => true
      })
  }

  def isSubtypeOf(other : AGNode[JavaNodeKind]) = {
    (this.kind, other.kind) match {
      case (Class(), Interface()) => new JavaType(this).subtypeOf(new JavaType(other))
      case (Class(), Class()) => new JavaType(this).subtypeOf(new JavaType(other))
      case _ => false
    }
  }

  /*  override def searchExistingAbstractions() = {
      val abstractionSet =  smutable.Set[(AGNode, AbstractionPolicy)]()
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
          }) {
          println("found " + n)
          abstractionSet += ((n, SupertypeAbstraction()))
        }
      }
      println("search terminated")
      abstractionSet
    }*/

  override def abstractionName(abskind :  JavaNodeKind, policy : AbstractionPolicy) : String =
    if(this.kind == Constructor())
      "create"
    else
      (abskind, policy) match {
        case (Method(), SupertypeAbstraction())
             | (AbstractMethod(), SupertypeAbstraction()) => this.name
        case _ => super.abstractionName(abskind, policy)

      }

  override def createAbstraction(abskind : JavaNodeKind,
                                 policy : AbstractionPolicy) = {
    (abskind, policy) match {
      case (Interface(), SupertypeAbstraction()) =>

        val abs = super.createAbstraction(Interface(), SupertypeAbstraction())

        content.foreach { child =>
          child.kind match {
            case ck @ Method() =>

              val absChild = child.createAbstraction(JavaNodeKind.abstractMethod(ck.`type`),
                SupertypeAbstraction())
              abs.content += absChild

              val absChildKind = absChild.kind.asInstanceOf[HasType[MethodType]]
              absChildKind.`type` = absChildKind.`type` copyWith this replacedBy abs

            case AbstractMethod() => throw new AGError("unhandled case !")
            case _ => ()
          }
        }
        this.superTypes_+=(abs)

        content.foreach { child =>
          child.kind match {
            // even fields can need to be promoted if they are written
            //case Field() =>
            case AbstractMethod() => throw new AGError("unhandled case !")
            case ck @ Method() =>
              val tcopy = ck.`type` copyWith this replacedBy abs
              ck.`type` = new MethodType(tcopy.input, ck.`type`.output)

              //we will do a second pass to see if the loss of information
              // in the output is really necessary

              /*content.foreach { sibling =>
                if(child != sibling){
                  if(child uses sibling &&
                    child.primaryUses.getOrEmpty(sibling).nonEmpty){
                    val siblingAbs = sibling.abstractions.find{
                      case (n, SupertypeAbstraction()) => abs.contains(n)
                      case _ => false
                    } match {
                      case Some(s) => s
                      case None => assert(false)
                    }
                    child.redirectUses(sibling, siblingAbs)
                  }


                }
              }*/

              if(child uses this) {
                graph.logger.writeln("interface creation : redirecting %s target to %s".format(AGEdge.uses(child, this), abs), 3)
                child.redirectUses(this, abs, SupertypeAbstraction())
              }
            case _ => ()

          }
        }

        abs

      case (AbstractMethod(), SupertypeAbstraction()) =>
        //no (abs, impl) or (impl, abs) uses
        createNodeAbstraction(abskind, policy)

      case _ =>
        val abs = super.createAbstraction(abskind, policy)
        this.kind match {
          case c @ Constructor() =>
            abs.kind.asInstanceOf[ConstructorMethod].ctorDecl = c.decl
          case _ => ()
        }
        abs
    }
  }

  override def moveTo(newContainer : AGNode[JavaNodeKind]) {
    //    println("moving " + this +" from " + container_! + " to " + newContainer)
    val oldContainer = container
    this.kind match{
      case Method()
           // | AbstractMethod() // ?
           | Field() =>
        this.users.foreach{ user =>
          /* We create a use toward the container that will be redirected
           * when redirecting primary uses
           */
          if(user.container == this.container){
            //need to create dependency before edge to apply "well"
            graph.addUsesDependency(user, user.container,
              user, this)
            AGEdge.uses(user, user.container).create()

          }
          if(container.uses(this)){
            graph.addUsesDependency(oldContainer, oldContainer,
              oldContainer, this)
          }
        }
      case _ => ()
    }


    super.moveTo(newContainer)
  }

  override def redirectUses(oldUsee : NodeType, newUsee : NodeType,
                            policy : RedirectionPolicy) = {

    (oldUsee.kind, newUsee.kind) match {
      case (Constructor(), Method())
           | (Constructor(), AbstractMethod())=>
        this.users.foreach{ AGEdge.uses(_,oldUsee).create()}
      case _ => ()
    }

    super.redirectUses(oldUsee, newUsee, policy)
  }

}