package puck.javaAG.immutable.nodeKind

import puck.graph.immutable.AccessGraph
import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.AGError
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}

/**
 * Created by lorilan on 31/07/14.
 */
abstract class TypeKind extends JavaNodeKind {
  def decl : Option[AST.TypeDecl]
  def createLockedAccess() : AST.Access = decl.get.createLockedAccess()
  def addDeclToProgram(prog : AST.Program, graph : AccessGraph[JavaNodeKind]){
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
        println("setting pathname with " + cpath)
        cu.setPathName(names.mkString(java.io.File.separator))
        cu.setTypeDecl(decl, 0)
        cu.setFromSource(true)
        prog.addCompilationUnit(cu)
    }
  }

}


case class Interface private[javaAG](node : NodeId[JavaNodeKind],
                                     decl : Option[AST.TypeDecl]) extends TypeKind {



  def isMergingCandidate(itc : Interface): Boolean = ??? /*{

    def hasMatchingMethod(absm : AGNode[JavaNodeKind])= absm.kind match{
      case absMethKind@AbstractMethod() =>
        absMethKind.findMergingCandidate(itc.node) match {
          case None => false
          case Some(_) => true
        }
      case _ => throw new AGError("Interface should contain only abstract method !!")

    }

    val otherItc = itc.node

    otherItc.content().size >= node.content().size &&
      (node.content() forall hasMatchingMethod) &&
        (otherItc.content().size == node.content().size ||
        {
          //otherItc has more methods, it is a potential subtype
          node.subTypes() forall otherItc.isSuperTypeOf
          //TODO structual type check
          /*val missingMethodsInThis =
            otherItc.content.filterNot{hasMatchingMethodIn(this)}
        })
  }*/*/

  override val toString = "Interface"

  def create(node : NodeId[JavaNodeKind]) = Interface(node, None)

  override def createDecl(prog : AST.Program,
                          graph : AccessGraph[JavaNodeKind]) : AccessGraph[JavaNodeKind] = {

    decl match {
      case None =>
        val itc = Interface(node, Some(new AST.InterfaceDecl()))
        itc.addDeclToProgram(prog, graph)
        graph.setKind(node, itc).graph
      case Some(_) => graph
    }
  }

  def canContain(k : JavaNodeKind) : Boolean = {
    k match {
      case _ : AbstractMethod => true
      case _ => false
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(JavaNodeKind.interface)
    case DelegationAbstraction() => List(JavaNodeKind.classKind)//also interface ?
  }
}

case class Class private[javaAG](node : NodeId[JavaNodeKind],
                                 decl : Option[AST.ClassDecl]) extends TypeKind {

  override val toString = "Class"

  def create(node : NodeId[JavaNodeKind]) = Class(node, None)

  override def createDecl(prog : AST.Program,
                          graph : AccessGraph[JavaNodeKind]) : AccessGraph[JavaNodeKind] = {

    decl match {
      case None =>
        val cls = Class(node, Some(new AST.ClassDecl()))
        cls.addDeclToProgram(prog, graph)
        graph.setKind(node, cls).graph
      case Some(_) => graph
    }
  }

  def canContain(k : JavaNodeKind) : Boolean = {
    k match {
      case _ : Constructor
           | _ : Field
           | _ : Method => true
      case _ => false
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(JavaNodeKind.interface, JavaNodeKind.classKind)
    case DelegationAbstraction() => List(JavaNodeKind.classKind)//also interface ?
  }

  //TODO prendre en compte le cas des classes abstraite
  def implements(itc : Interface): Unit = {


  }

  /*override def promoteToSuperTypeWherePossible(superType : AGNode[JavaNodeKind]){
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