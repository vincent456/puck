package puck.javaGraph

import puck.PuckError
import puck.graph._
import puck.graph.constraints.DelegationAbstraction
import puck.javaGraph.nodeKind._

import ShowDG._

object ASTNodeLink{

  type NodeT = ConcreteNode

  def createNewInstanceExpr
  ( field : AST.FieldDeclaration,
    cdecl : AST.ConstructorDecl
    ) : Unit = {
    val expr = new AST.ClassInstanceExpr()
    expr.setAccess(cdecl.hostType().createLockedAccess())
    field.setInit(expr)
  }


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
    graph.styp(node.id) match {
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
    graph.styp(node.id) match {
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

    graph.styp(node.id) match {
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

  def setName(name : String, nl : ASTNodeLink) : Unit = nl match {
    case FieldDeclHolder(decl) => decl.setID(name)
    case dh : MethodDeclHolder => dh.decl.setID(name)
    case th : TypedKindDeclHolder => th.decl.setID(name)
    case ch : ConstructorDeclHolder => ch.decl.setID(name)
    case h => throw new PuckError(h.getClass + " setName unhandled")
  }
}

sealed trait ASTNodeLink

case object NoDecl extends ASTNodeLink
case object PackageDeclHolder extends ASTNodeLink

sealed abstract class DefHolder extends ASTNodeLink {
  def node : AST.ASTNode[_]
}
case class ExprHolder(expr : AST.Expr) extends DefHolder{
  def node = expr.asInstanceOf[AST.ASTNode[_]]
}
case class BlockHolder(block : AST.Block) extends DefHolder{
  def node = block.asInstanceOf[AST.ASTNode[_]]
}

case class ParameterDeclHolder(decl : AST.ParameterDeclaration) extends ASTNodeLink

sealed trait HasBodyDecl extends ASTNodeLink{
  val decl : AST.BodyDecl
}

sealed trait HasMemberDecl extends HasBodyDecl{
  override val decl : AST.MemberDecl
}


class DeclarationCreationError(msg : String) extends DGError(msg)

case class ConstructorDeclHolder(decl : AST.ConstructorDecl) extends HasBodyDecl

case class FieldDeclHolder(decl : AST.FieldDeclaration) extends HasMemberDecl

object MethodDeclHolder {
  def unapply(mdh : MethodDeclHolder) : Some[AST.MethodDecl] =
    Some(mdh.decl)
}
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
case class ClassDeclHolder(decl : AST.ClassDecl) extends TypedKindDeclHolder
case class WildCardTypeHolder(decl : AST.WildcardType) extends TypedKindDeclHolder
case class TypeVariableHolder(decl : AST.TypeVariable) extends TypedKindDeclHolder
case class PrimitiveDeclHolder(decl : AST.TypeDecl) extends TypedKindDeclHolder