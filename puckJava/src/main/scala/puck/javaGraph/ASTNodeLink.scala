package puck.javaGraph

import puck.PuckError
import puck.graph._
import puck.graph.constraints.DelegationAbstraction
import puck.javaGraph.nodeKind._

import ShowDG._

object ASTNodeLink{

  type NodeT = ConcreteNode

  def createInterface
  ( prog : AST.Program,
    graph : DependencyGraph,
    node : NodeT
    ) : InterfaceDeclHolder = {
    val itc = InterfaceDeclHolder(new AST.InterfaceDecl())
    createTypeDecl(itc.decl, prog, graph, node)
    itc
  }

  def createClass
  ( prog : AST.Program,
    graph : DependencyGraph,
    node : NodeT
    ) : ClassDeclHolder = {
    val cls = ClassDeclHolder(new AST.ClassDecl())
    createTypeDecl(cls.decl, prog, graph, node)
    cls
  }

    def createTypeDecl
  ( decl : AST.TypeDecl,
    prog : AST.Program,
    graph : DependencyGraph,
    node : NodeT) : Unit = {
    decl.setID(node.name)
    decl.setModifiers(new AST.Modifiers("public"))
    //package en path will be set when the contains arc will be add to the graph
    prog.insertUnusedType("", "", decl)
    ()
  }

  def createAbstractMethod
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : NodeT
    ) : AbstractMethodDeclHolder = {
    node.styp match {
      case Some(mt : MethodType) =>
        val m =
          AST.MethodDecl.createAbstractMethod(
            mt.createReturnAccess(graph, id2Decl),
            node.name,
            mt.createASTParamList(graph, id2Decl).toArray)
        AbstractMethodDeclHolder(m)

      case _ => throw new DeclarationCreationError(" not a method type !!")

    }
  }

  def createConstructorMethod
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : NodeT
    ) : ConstructorMethodDeclHolder = {
    val someKtor = graph.container(node.id).flatMap( graph.content(_).find{ n0 =>
      val n1 = graph.getConcreteNode(n0)
      n1.kind == Constructor &&
        graph.abstractions(n0).exists {
          case AccessAbstraction(n2 , DelegationAbstraction) => n2 == node.id
          case _ => false
        }
    })

    someKtor match {
      case None => throw new DeclarationCreationError("no constructor found")
      case Some(c) =>
        val ktor = id2Decl(c).asInstanceOf[ConstructorDeclHolder]
        val decl = ktor.decl.createConstructorMethod(node.name)
        ConstructorMethodDeclHolder(decl, ktor.decl)

    }

  }

  def createConstructor
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : NodeT
    ) : ConstructorDeclHolder = {
    node.styp match {
      case Some( mt @ MethodType(_,_)) =>
        ConstructorDeclHolder(AST.ConstructorDecl.
          createConstructor(mt.createReturnAccess(graph, id2Decl), node.name,
            mt.createASTParamList(graph, id2Decl).toArray))
      case _ => throw new DeclarationCreationError(" not a constructor type !!")
    }
  }

  def createField
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : NodeT
    ) : FieldDeclHolder = {

    node.styp match {
      case Some(NamedType(id)) =>
        id2Decl get id match {
          case Some(tdh : TypedKindDeclHolder) =>
            import AST._
            val f =
              new FieldDeclaration(
                new Modifiers(ASTNode.VIS_PRIVATE),
                tdh.decl.createLockedAccess(),
                node.name)

            FieldDeclHolder(f)
          case declHolder =>
            throw new DeclarationCreationError(s"${showDG[NodeId](graph).shows(id)} is not a type !! (styp = $declHolder})")
        }
      case _ => throw new DeclarationCreationError(" not a field type !!")
    }
  }

  def createDecl(prog : AST.Program,
                 graph : DependencyGraph,
                 id2decl : Map[NodeId, ASTNodeLink],
                 node : NodeT) : ASTNodeLink = {
    node.kind match {
      case Package => PackageDeclHolder
      case Interface => createInterface(prog, graph, node)
      case Class => createClass(prog, graph, node)
      case ConstructorMethod =>
        createConstructorMethod(prog, graph, id2decl, node)
      case AbstractMethod =>
        createAbstractMethod(prog, graph, id2decl, node)
      case Constructor =>
        createConstructor(prog, graph, id2decl, node)
      case Field => createField(prog, graph, id2decl, node)

      case _ => throw new DeclarationCreationError(s"cannot create decl for kind ${node.kind}")

    }
  }

  val setName : String => ASTNodeLink => Unit = name => {
    case FieldDeclHolder(decl) => decl.setID(name)
    case dh : MethodDeclHolder => dh.decl.setID(name)
    case th : TypedKindDeclHolder => th.decl.setID(name)
    case _ : ConstructorDeclHolder => () //should be handled in the class rename
    case h => throw new PuckError(h.getClass + " setName unhandled")
  }
}

sealed trait ASTNodeLink

case object NoDecl extends ASTNodeLink
case object PackageDeclHolder extends ASTNodeLink

sealed trait HasBodyDecl extends ASTNodeLink{
  val decl : AST.BodyDecl
}

sealed trait HasMemberDecl extends HasBodyDecl{
  override val decl : AST.MemberDecl
}


class DeclarationCreationError(msg : String) extends DGError(msg)

case class ConstructorDeclHolder(decl : AST.ConstructorDecl) extends HasBodyDecl

case class FieldDeclHolder(decl : AST.FieldDeclaration) extends HasMemberDecl

trait MethodDeclHolder extends HasMemberDecl {
  val decl : AST.MethodDecl
}

case class ConcreteMethodDeclHolder(decl : AST.MethodDecl) extends MethodDeclHolder

case class AbstractMethodDeclHolder(decl : AST.MethodDecl) extends MethodDeclHolder

case class ConstructorMethodDeclHolder( decl : AST.MethodDecl,
                                        ctorDecl : AST.ConstructorDecl) extends MethodDeclHolder

trait TypedKindDeclHolder extends ASTNodeLink {
  def decl : AST.TypeDecl
}

case class InterfaceDeclHolder(decl : AST.InterfaceDecl) extends TypedKindDeclHolder

case class ClassDeclHolder(decl : AST.ClassDecl) extends TypedKindDeclHolder {
  /*override def promoteToSuperTypeWherePossible(superType : AGNode){
   val implementor = this.node

   superType.content foreach { absMethod =>
     absMethod.kind match {
       case absMethKind @ AbstractMethod() =>
         implementor.content find { c =>
           c.kind match {
             case implKind @ Method() =>
               absMethKind.`type` == implKind.`type`
             case _ => false
           }
         } match {
           case None => throw new AGError("Interface has a method not implemented") //what if implementor is an abstract class ?
           case Some(impl) =>

             absMethKind.`type` = absMethKind.`type` copyWith implementor replacedBy superType

             impl.kind match {
               case m @ Method() => m.`type` = new MethodType(absMethKind.`type`.copy().input,
                 m.`type`.output)
               case _ => assert(false)
             }

             impl.users.foreach{ user =>
               val primUses = user.primaryUses.getOrEmpty(impl)
               //if a method use has no dominant use it must be
               if(primUses.nonEmpty){
                 user.redirectUses(implementor, superType, SupertypeAbstraction())
               }
             }
         }

       case othk => throw new AGError("interface should contains only abstract method !!! contains : " + absMethod)
     }
   }

 }*/
}

case class WildCardTypeHolder(decl : AST.WildcardType) extends TypedKindDeclHolder
case class TypeVariableHolder(decl : AST.TypeVariable) extends TypedKindDeclHolder
case class PrimitiveDeclHolder(decl : AST.TypeDecl) extends TypedKindDeclHolder