package puck.javaAG

import puck.graph._
import puck.graph.constraints.DelegationAbstraction
import puck.javaAG.nodeKind._


object ASTNodeLink{

  type NodeT = DGNode


  def addTypeDeclToProgram(decl : AST.TypeDecl,
                           prog : AST.Program,
                           graph : DependencyGraph,
                           node : NodeT){
    decl.setID(node.name)
    decl.setModifiers(new AST.Modifiers("public"))
    //package en path will be set when the contains arc will be add to the graph
    prog.insertUnusedType("", "", decl)

  }

  def createConstructorMethodDecl(prog : AST.Program,
                                  graph : DependencyGraph,
                                  id2Decl : Map[NodeId, ASTNodeLink],
                                  node : NodeT) : ConstructorMethodDeclHolder = {
    val someKtor = graph.container(node.id).flatMap( graph.content(_).find{ n0 =>
      val n1 = graph.getNode(n0)
      n1.kind == Constructor &&
        graph.abstractions(n0).exists {
          case (n2 , DelegationAbstraction) => n2 == node.id
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

  def createDecl(prog : AST.Program,
                 graph : DependencyGraph,
                 id2decl : Map[NodeId, ASTNodeLink],
                 node : NodeT) : ASTNodeLink = {
    node.kind match {
      case Package => PackageDeclHolder
      case Interface =>
        val itc = InterfaceDeclHolder(new AST.InterfaceDecl())
        addTypeDeclToProgram(itc.decl,prog, graph, node)
        itc
      case Class =>
        val cls = ClassDeclHolder(new AST.ClassDecl())
        addTypeDeclToProgram(cls.decl, prog, graph, node)
        cls
      case ConstructorMethod =>
        createConstructorMethodDecl(prog, graph, id2decl, node)
      case AbstractMethod =>
        node.styp match {
          case MethodTypeHolder(arrow) =>
            //TODO remove asInstanceOf
            val mt = arrow.asInstanceOf[MethodType]

            AbstractMethodDeclHolder(AST.MethodDecl.createAbstractMethod(mt.createReturnAccess(graph, id2decl),
              node.name, mt.createASTParamList(graph, id2decl).toArray))

          case _ => throw new DeclarationCreationError(" not a method type !!")

        }

      case _ => throw new DeclarationCreationError(s"cannot create decl for kind ${node.kind}")

    }
  }
}

sealed trait ASTNodeLink

case object NoDecl extends ASTNodeLink
case object PackageDeclHolder extends ASTNodeLink

sealed trait HasBodyDecl extends ASTNodeLink{
  val decl : AST.BodyDecl
}

class DeclarationCreationError(msg : String) extends AGError(msg)

case class ConstructorDeclHolder(decl : AST.ConstructorDecl) extends HasBodyDecl
case class FieldDeclHolder(decl : AST.FieldDeclaration) extends HasBodyDecl

trait MethodDeclHolder extends HasBodyDecl {
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

case class TypeVariableHolder(decl : AST.TypeVariable) extends TypedKindDeclHolder
case class PrimitiveDeclHolder(decl : AST.TypeDecl) extends TypedKindDeclHolder