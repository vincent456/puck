package puck.javaAG.mutable

import AST.CompilationUnit
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.mutable.backTrack._
import puck.graph.mutable._
import puck.javaAG.JavaAGError
import puck.javaAG.mutable.nodeKind._
import puck.util.{PuckLog, PuckLogger, PuckNoopLogger}

/**
 * Created by lorilan on 23/07/14.
 */
object AG2AST {
  var logger : PuckLogger = PuckNoopLogger
  implicit val defaultVerbosity = (PuckLog.AG2AST, PuckLog.Info)

  def apply(t : Recordable[JavaNodeKind]) = t match {
    case Transformation(Add(), TTNode(node)) =>
      //println("creating node " + node)
      add(node)

    case Transformation(Add(), TTEdge(e)) =>
      //println("creating edge " + e)
      add(e)

    case Transformation(Add(), TTRedirection(e, Target(newTarget))) =>
      logger.writeln("redirecting %s target to %s".format(e, newTarget))
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

    case Transformation(_, TTAbstraction(impl, abs, SupertypeAbstraction)) =>
      (impl.kind, abs.kind) match {
        case (m @ Method(), AbstractMethod()) =>
            m.decl.setVisibility(AST.ASTNode.VIS_PUBLIC)
        case _ => ()
      }

    case Transformation(_, TTAbstraction(_, _, _)) => ()
    case Transformation(_, TTDependency(_,_)) => ()
    case UndoBreakPoint(_) => ()
    //case Transformation(_, TTConstraint(_,_)) =>

    case Transformation(Remove(), TTNode(node)) =>
      node.kind match {
        case k : TypeKind => k.decl.puckDelete()
        case _ => logger.writeln("%s not applied on program".format(t))
      }


    case _ => logger.writeln("%s not applied on program".format(t))
  }


  def add(node : AGNode[JavaNodeKind]){ node.kind.createDecl(node) }

  def add(e : AGEdge[JavaNodeKind]) = e.kind match {

    case Contains() =>
      (e.source.kind, e.target.kind) match {
        case (Package(), i: TypeKind) =>
          val cu = i.decl.compilationUnit()


          val cpath = e.source.containerPath.map(_.name)
          val sepPath = cpath.tail.mkString(java.io.File.separator)

          val relativePath = sepPath + java.io.File.separator + e.target.name + ".java"
          cu.setPathName( relativePath )
          cu.setRelativeName(relativePath) // weird but seems to be the default behavior
          cu.setPackageDecl(e.source.fullName)

        case (i@Interface(), m@AbstractMethod()) =>
          i.decl.addBodyDecl(m.decl)

        case (c@Class(), m@Method()) =>
          c.decl.addBodyDecl(m.decl)


        case (Package(), Package()) => ()// can be ignored

        case _ => logger.writeln("%s not created".format(e))

      }

    case Isa() =>
      (e.source.kind, e.target.kind) match {
        case (c@Class(), i@Interface()) =>
          c.decl.addImplements(i.createLockedAccess())
        case _ => logger.writeln("%s not created".format(e))
      }

    case Uses() =>

      (e.source.kind, e.target.kind) match {
        case (cm : ConstructorMethod, Constructor()) => ()//already generated when creating ConstructorMethod decl
        case (Class(), Interface()) => logger.writeln("do not create %s : assuming its an isa edge (TOCHECK)".format(e)) // class imple

        /*case (f @ Field(), k : TypeKind) =>
          f.decl.setTypeAccess(k.lockedAccess())

        case ( m @ Method(), k : TypeKind ) =>
          m.`type`.input.types.foreach{
            case NamedType(n) =>

          }*/
        case _ => logger.writeln(" =========> need to create " + e)
      }

  }


  def redirectTarget(e : AGEdge[JavaNodeKind], newTarget : AGNode[JavaNodeKind]){
    (e.target.kind, newTarget.kind) match {
      case ( k @ Interface(), newk @ Interface()) if e.kind == Isa() =>
        e.source.kind match {
          case src @ Class() =>
           src.decl.replaceImplements(k.createLockedAccess(), newk.createLockedAccess())

          case _ => throw new JavaAGError("isa arc should only be between TypeKinds")
        }

      case (oldk: TypeKind, newk: TypeKind) =>
        e.source.kind match {
          case f @ Field() =>
            f.decl.replaceTypeAccess(oldk.createLockedAccess(), newk.createLockedAccess())

          case m @ Method() =>
            m.decl.replaceTypeAccess(oldk.createLockedAccess(), newk.createLockedAccess())

          case m @ AbstractMethod() =>
            m.decl.replaceTypeAccess(oldk.createLockedAccess(), newk.createLockedAccess())

          case Class() =>
            e.target.graph.logger.writeln("Class user of TypeKind, assume this is the \"doublon\" of " +
              "an isa arc, redirection ignored",1)

          case k => throw new JavaAGError(k + " as user of TypeKind, redirection unhandled !")
        }

      case (oldk: MethodKind, newk: MethodKind) =>

        e.source.kind match {
          case c @ Constructor() =>
            c.decl.replaceMethodCall(oldk.decl, newk.decl)
          case m @ Method() =>
            m.decl.replaceMethodCall(oldk.decl, newk.decl)
          case k => throw new JavaAGError(k + " as user of MethodKind, redirection unhandled !")
        }

      case (oldk @ Constructor(), newk : ConstructorMethod) =>
        e.source.kind match {
          case c @ Constructor() =>
            throw new JavaAGError("redirection to constructor method within constructor no implemented (see methodDecl)")

          case m @ Method() =>
            m.decl.replaceByConstructorMethodCall(newk.decl)

          case k => throw new JavaAGError(k + " as user of MethodKind, redirection unhandled !")
        }

      case _ => throw new JavaAGError("redirecting TARGET of %s to %s : application failure !".format(e, newTarget))
    }
  }


  def redirectSource(e : AGEdge[JavaNodeKind], newSource : AGNode[JavaNodeKind]){
    import AST.ASTNode.VIS_PUBLIC

    if (e.source != newSource)//else do nothing
      (e.source.kind, newSource.kind, e.target.kind) match {
        case (c1 @ Class(), c2 @ Class(), m @ Method()) =>
          c1.decl.removeBodyDecl(m.decl)
          c2.decl.addBodyDecl(m.decl)
          //TODO fix : following call create a qualifier if it is null
          //the qualifier is a parameter for which a new instance is created
          //in some case it changes the meaning of the program !!
          c1.decl.replaceMethodCall(m.decl, m.decl)

          if(c1.decl.getVisibility != VIS_PUBLIC){
            m.node.users().find{_.kind.packageNode != c2.packageNode} match {
              case Some(_) => m.decl.setVisibility(VIS_PUBLIC)
              case None =>
            }
          }

        case (p1 @ Package(), p2 @ Package(), i: TypeKind) =>
          /*println("moving typedecl %s of package (cu contains %d typedecl)".format(e.target.fullName,
            i.decl.compilationUnit().getNumTypeDecl))*/

          //println("moving " + i +" from package "+ p1 +" to package" + p2)
          if(i.decl.compilationUnit().getNumTypeDecl > 1) {
            val oldcu = i.decl.compilationUnit()

            val rootPathName = oldcu.getRootPath
            oldcu.removeTypeDecl(i.decl)

            val path =  rootPathName + newSource.fullName . replaceAllLiterally(".", java.io.File.separator) +
              java.io.File.separator

            val newCu = new CompilationUnit()
            oldcu.programRoot().insertUnusedType(path, newSource.fullName, i.decl)

            /*import scala.collection.JavaConversions.asScalaIterator
            asScalaIterator(oldcu.getImportDeclList.iterator).foreach{newCu.addImportDecl}*/
          }
          else
            i.decl.compilationUnit().setPackageDecl(newSource.fullName)

          if(i.decl.getVisibility != VIS_PUBLIC ){
            i.node.users().find{_.kind.packageNode != p2.node} match {
              case Some(_) => i.decl.setVisibility(VIS_PUBLIC)
              case None => ()
            }
          }


        case _ =>throw new JavaAGError("redirecting SOURCE of %s to %s : application failure !".format(e, newSource))
      }
  }
}
