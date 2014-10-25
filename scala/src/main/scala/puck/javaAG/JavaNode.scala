package puck.javaAG

import puck.graph._
import puck.graph.constraints.{RedirectionPolicy, AbstractionPolicy, SupertypeAbstraction}
import puck.graph.io.DotHelper
import puck.javaAG.nodeKind._
import puck.util.PuckLog


/**
 * Created by lorilan on 06/05/14.
 */


object JavaNode extends DotHelper[JavaNodeKind] with AGNodeBuilder[JavaNodeKind]{
  def isDotSubgraph( k : JavaNodeKind ) = k match {case Package() => true; case _ => false}
  def isDotClass( k : JavaNodeKind ) = k match { case Class() | Interface() => true; case _ => false}
  def fillColor( k : JavaNodeKind ) = k match {
    case Package() => "#FF9933" //Orange
    case Interface() => "#FFFF99" // Light yellow
    case Class() | Constructor() => "#FFFF33" //Yellow
    case Method() | Field() => "#FFFFFF" //White
    case Literal() => "#CCFFCC" //Very Light green
    case _ => throw new Error("Unknown JavaNodeKind")
  }

  def rootKind = JavaRoot()

  def namePrefix( k : JavaNodeKind ) = k match {
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
                kind : JavaNodeKind )
  extends AGNode[JavaNodeKind](graph, id, name, kind){

  implicit val defaultVerbosity = (InJavaGraph(), PuckLog.Info())

  override def canContain(n : AGNode[JavaNodeKind]) : Boolean = {

    def noNameClash( l : Int )( c : AGNode[JavaNodeKind] ) : Boolean =
      c.kind match {

      case ck : MethodKind  =>
        c.name != n.name || {

          if(ck.typ == null)
            println("type is null")

          ck.typ.input.length != l}
      case _ => true
    }

    super.canContain(n) &&
      (n.kind match {
        case nk@AbstractMethod() =>
          /*
            All subtypes must implement the method
           */
          this.content.forall(noNameClash(nk.typ.input.length)) &&
            this.directSubTypes.forall {
              _.content.exists { c =>
                c.kind match {
                  case ck@Method() => n.name == c.name && nk.typ == ck.typ
                  case _ => false
                }
              }
            }
        /*
          cannot have two methods with same name and same type
          */
        case nk @ Method() =>
          this.content.forall(noNameClash(nk.typ.input.length))

        case _ => true
      })
  }

  /* def isSubtypeOf(other : AGNode[JavaNodeKind]) = {
     (this.kind, other.kind) match {
       case (Class(), Interface()) => new JavaNamedType(this).subtypeOf(new JavaNamedType(other))
       case (Class(), Class()) => new JavaNamedType(this).subtypeOf(new JavaNamedType(other))
       case _ => false
     }
   }*/


  //A merging candidate is either structurally equal
  //either a subtype of this
  //hence if we do the merge "this" will disappear
  // and all its user redirected to the candidate
  override def findMergingCandidate() : Option[AGNode[JavaNodeKind]] = this.kind match{

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
  }



  /*override def searchExistingAbstractions() = this.kind match {
    case Class() =>
      val abstractionSet =  smutable.Set[(AGNode, AbstractionPolicy)]()
      println("searching abstractions for " + this)
      graph.iterator foreach { n =>
      /*if(n != this && (this isSubtypeOf n) &&
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
      }*/
    }
    println("search terminated")
    abstractionSet
    case _ => super.searchExistingAbstractions()
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
            //case ck @ Method() =>
            case ck : MethodKind =>
              val absChild = child.createAbstraction(JavaNodeKind.abstractMethod(ck.typ),
                SupertypeAbstraction())
              abs.content += absChild

              val absChildKind = absChild.kind.asInstanceOf[AbstractMethod]
              absChildKind.typ = absChildKind.typ copyWith this replacedBy abs

            //case AbstractMethod() => throw new AGError("unhandled case !")
            case _ => ()
          }
        }
        this.superTypes_+=(abs)

        content.foreach { child =>
          child.kind match {
            // even fields can need to be promoted if they are written
            //case Field() =>
            case ck : MethodKind =>
            /*case AbstractMethod() => throw new AGError("unhandled case !")
            case ck @ Method() =>*/
              val tcopy = ck.typ copyWith this replacedBy abs
              ck.typ = new MethodType(tcopy.input, ck.typ.output)

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
                graph.redirectUses(AGEdge.uses(child, this), abs, SupertypeAbstraction())
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

  /*override def moveTo(newContainer : AGNode[JavaNodeKind]) {
    graph.logger.writeln("moving " + this +" from " + container + " to " + newContainer)
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
  }*/

  //other is potentialy a superType and thus may have less methods than this
  override def mergeWith(other : AGNode[JavaNodeKind]){
    (this.kind, other.kind) match {
      case (Interface(), Interface()) =>
        //TODO see why the call to apply on content is needed (other.content.toList compile but doesn't give the right result)
        //the toList is necessary :
        // removing the firsts children (AGEdge.contains(other.container, other).delete() in AGNode.mergeWith)
        // modifies the content set and thus the iterator
        other.content().toList foreach { otherAbsMethod =>
          otherAbsMethod.kind match {
            case otherKind @ AbstractMethod() =>
              otherKind.findMergingCandidate(this) match {
                case Some(thisAbsMethod) => thisAbsMethod mergeWith otherAbsMethod
                case None => throw new Error(otherAbsMethod.fullName + " has no method to merge with")
              }
            case _ => assert(false)
          }
        }
        super.mergeWith(other)

      case (AbstractMethod(), AbstractMethod()) =>
        super.mergeWith(other)
      case _ => ()
    }
  }
}