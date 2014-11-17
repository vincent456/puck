package puck.javaAG.immutable
import puck.graph.AGError
import puck.graph.constraints.DelegationAbstraction
import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{Hook, AGNode, AccessGraph}
import puck.javaAG.immutable.nodeKind.{MethodTypeHolder, Constructor, JavaNodeKind}

sealed trait DeclHolder extends Hook{
  //def decl : Option[AST.ASTNode[_]] = throw new AGError("no declaration for " + this.getClass)

  def createDecl(prog : AST.Program,
                 graph : AccessGraph,
                 node : NodeId) : AccessGraph =
    throw new DeclarationCreationError(graph.getNode(node).toString)
}

case object EmptyDeclHolder extends DeclHolder

case object PackageDeclHolder extends DeclHolder {
  override def createDecl(prog : AST.Program,
                 graph : AccessGraph,
                 node : NodeId) : AccessGraph = graph

}
class DeclarationCreationError(msg : String) extends AGError(msg)

case class ConstructorDeclHolder(decl : Option[AST.ConstructorDecl]) extends DeclHolder

case class FieldDeclHolder(decl : Option[AST.FieldDeclaration]) extends DeclHolder

trait MethodDeclHolder extends DeclHolder {
  val decl : Option[AST.MethodDecl]
}
case class ConcreteMethodDeclHolder(decl : Option[AST.MethodDecl]) extends MethodDeclHolder {
  override def createDecl(prog : AST.Program,
                          graph : AccessGraph,
                          node : NodeId) : AccessGraph = {

    decl match {
      case None =>
        val n = graph.getNode(node)

        val someKtor = graph.getNode(n.container).content.find{ n0 =>
          val n1 = graph.getNode(n0)
          n1.kind == Constructor &&
            n1.abstractions.exists {
              case (n2 , DelegationAbstraction) => n2 == node
              case _ => false
            }
        }

        someKtor match {
          case None => throw new DeclarationCreationError("no constructor found")
          case Some(c) =>
            val ktor = graph.getNode(c).styp.asInstanceOf[ConstructorMethodDeclHolder]
            val decl = ktor.ctorDecl.map(_.createConstructorMethod(n.name))
            graph.setInternal(node, ConcreteMethodDeclHolder(decl))
        }

      case Some(_) => graph
    }
  }
}

case class AbstractMethodDeclHolder(decl : Option[AST.MethodDecl]) extends MethodDeclHolder {
  override def createDecl(prog : AST.Program,
                          graph : AccessGraph,
                          nodeId : NodeId) : AccessGraph = {
    val node = graph.getNode(nodeId)
    (decl, node.styp) match {
      case (None, MethodTypeHolder(arrow)) =>
        val mt = arrow.asInstanceOf[MethodType]
        mt.createReturnAccess(graph) match {
          case Some(access) =>
            val decl = AST.MethodDecl.createAbstractMethod(access,
              graph.getNode(nodeId).name, mt.createASTParamList(graph).toArray)
            graph.setInternal(nodeId, AbstractMethodDeclHolder(Some(decl)))
          case _ => throw new AGError()
        }

      case (None, _) => throw new AGError(" not a method type !!")
      case (Some(_),_) => graph
    }
  }
}

case class ConstructorMethodDeclHolder( decl : Option[AST.MethodDecl],
                                  ctorDecl : Option[AST.ConstructorDecl]) extends MethodDeclHolder {

  override def createDecl(prog : AST.Program,
                          graph : AccessGraph,
                          node : NodeId) : AccessGraph = {
    decl match {
      case None =>
        val name = graph.getNode(node).name
        val decl = Some(ctorDecl.get.createConstructorMethod(name))
        graph.setInternal(node, ConstructorMethodDeclHolder(decl, ctorDecl))
      case Some(_) => graph
    }
  }
}

trait TypedKindDeclHolder extends DeclHolder {
  def decl : Option[AST.TypeDecl]
  def createLockedAccess() : Option[AST.Access] = decl.map(_.createLockedAccess())
  def addDeclToProgram(prog : AST.Program,
                       graph : AccessGraph,
                       node : NodeId){
    /*val prog = node.graph.root.kind match {
      case r @ JavaRoot() => r.program
      case r => throw new Error("root should be of kind JavaRoot instead of " + r)
    }*/
    decl match {
      case None => ()
      case Some(decl) =>
        val node0 = graph.getNode(node)
        decl.setID(node0.name)
        decl.setModifiers(new AST.Modifiers("public"))
        val cu = new AST.CompilationUnit()
        cu.setRelativeName(node0.name)

        val cpath = node0.containerPath
        if(! graph.getNode(cpath.head).isRoot)
          throw new AGError("cannot create decl for unrooted node")

        val names = cpath.tail.map(graph.getNode(_).name)
        println(s"setting pathname with $cpath")
        cu.setPathName(names.mkString(java.io.File.separator))
        cu.setTypeDecl(decl, 0)
        cu.setFromSource(true)
        prog.addCompilationUnit(cu)
    }
  }
}

case class InterfaceDeclHolder(decl : Option[AST.InterfaceDecl]) extends TypedKindDeclHolder {

  override def createDecl(prog : AST.Program,
                          graph : AccessGraph,
                          node : NodeId) : AccessGraph = {

    decl match {
      case None =>
        val itc = InterfaceDeclHolder(Some(new AST.InterfaceDecl()))
        itc.addDeclToProgram(prog, graph, node)
        graph.setInternal(node, itc)
      case Some(_) => graph
    }
  }
}

case class ClassDeclHolder(decl : Option[AST.ClassDecl]) extends TypedKindDeclHolder {
  override def createDecl(prog : AST.Program,
                          graph : AccessGraph,
                          node : NodeId) : AccessGraph = {

    decl match {
      case None =>
        val cls = ClassDeclHolder(Some(new AST.ClassDecl()))
        cls.addDeclToProgram(prog, graph, node)
        graph.setInternal(node, cls)
      case Some(_) => graph
    }
  }

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

case class PrimitiveDeclHolder(decl : Option[AST.TypeDecl]) extends TypedKindDeclHolder