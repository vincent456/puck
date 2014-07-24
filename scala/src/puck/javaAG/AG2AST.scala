package puck.javaAG

import puck.graph._
import puck.graph.backTrack._

/**
 * Created by lorilan on 23/07/14.
 */
object AG2AST {
  def apply(t : Recordable[JavaNodeKind])= t match {
    case Transformation(Add(), TTNode(node)) =>
      println("creating node " + node)
      add(node)

    case Transformation(Add(), TTEdge(e)) =>
      println("creating edge " + e)
      add(e)

    case Transformation(Add(), TTRedirection(e, Target(newTarget))) =>
      redirectTarget(e, newTarget)

    case Transformation(Add(), TTRedirection(e, Source(newSource))) =>
      redirectSource(e, newSource)
    /*case Transformation(Remove(), TTEdge(e@AGEdge(Contains(), source, target))) =>
    println("removing edge " + e)
    (source.kind, target.kind) match {
    case (p@Package(), i: TypeKind) =>
      i.decl.compilationUnit().setPackageDecl("unkown")

    case (i@Interface(), m@AbstractMethod()) =>
      i.decl.removeBodyDecl(m.decl)

    case (c@Class(), m@Method()) =>
      c.decl.removeBodyDecl(m.decl)

    case _ => println("%s not removed".format(e))

    }*/

    case r1 => () //println("%s not applied on program".format(r1))
  }


  def add(node : AGNode[JavaNodeKind]){ node.kind.createDecl(node) }

  def add(e : AGEdge[JavaNodeKind]) = e.kind match {

    case Contains() =>
      (e.source.kind, e.target.kind) match {
        case (p@Package(), i: TypeKind) =>
          i.decl.compilationUnit().setPackageDecl(e.source.fullName)

        case (i@Interface(), m@AbstractMethod()) =>
          i.decl.addBodyDecl(m.decl)

        case (c@Class(), m@Method()) =>
          c.decl.addBodyDecl(m.decl)

        case _ => println("%s not created".format(e))

      }

    case Isa() =>
      (e.source.kind, e.target.kind) match {
        case (c@Class(), i@Interface()) =>
          c.decl.addImplements(i.createLockedAccess())
        case _ => println("%s not created".format(e))
      }

    case Uses() if e.isDominant =>
      println("need to create " + e + " !!!!")
    /*(e.source.kind, e.target.kind) match {
        case (f @ Field(), k : TypeKind) =>
          f.decl.setTypeAccess(k.lockedAccess())

        case ( m @ Method(), k : TypeKind ) =>
          m.`type`.input.types.foreach{
            case NamedType(n) =>

          }

      }*/

    case Uses() => println(e + " is not a dominant uses")

    case Undefined() => println("cannot create " + e + " !!!")
  }


  def redirectTarget(e : AGEdge[JavaNodeKind], newTarget : AGNode[JavaNodeKind]){
    (e.target.kind, newTarget.kind) match {
      case (oldk: TypeKind, newk: TypeKind) =>
        e.source.kind match {
          case f@Field() =>
            f.decl.setTypeAccess(newk.createLockedAccess())

          case m@Method() =>
            m.decl.replaceAccess(oldk.createLockedAccess(), newk.createLockedAccess())

          case m@AbstractMethod() =>
            m.decl.replaceAccess(oldk.createLockedAccess(), newk.createLockedAccess())

          case k => throw new JavaAGError(k + " as user of TypeKind, redirection unhandled !")
        }

      case (oldk: MethodKind, newk: MethodKind) =>

        e.source.kind match {
          case c@Constructor() =>
            c.decl.replaceMethodCall(oldk.decl, newk.decl)
          case m@Method() =>
            m.decl.replaceMethodCall(oldk.decl, newk.decl)
          case k => throw new JavaAGError(k + " as user of MethodKind, redirection unhandled !")
        }

      case (oldk @ Constructor(), newK @ Method()) =>


      case _ => throw new JavaAGError("redirecting target of %s to %s : application failure !".format(e, newTarget))
    }
  }


  def redirectSource(e : AGEdge[JavaNodeKind], newSource : AGNode[JavaNodeKind]){
    (e.source.kind, newSource.kind, e.target.kind) match {
      case (c1@Class(), c2@Class(), m@Method()) =>

        c1.decl.removeBodyDecl(m.decl)
        //m.decl = m.decl.makeStatic() //makeStatic do a copy !!
        /*//JRRT cannot move instance methode
        m.decl.moveTo(c2.decl)*/
        //m.decl.getModifiers.removeModifier("static")
        //m.decl.getModifiers.flushCache()
        c2.decl.addBodyDecl(m.decl)

      case _ => throw new Error("both target should be typekind !")
    }
  }
}
