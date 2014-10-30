package puck.javaAG.mutable.nodeKind

import puck.graph.AGError
import puck.graph.mutable.AGNode
import puck.graph.mutable.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.javaAG.mutable.MethodType

/**
 * Created by lorilan on 31/07/14.
 */
abstract class TypeKind extends JavaNodeKind {
  def decl : AST.TypeDecl
  def createLockedAccess() : AST.Access = decl.createLockedAccess()
  def addDeclToProgram(){
    val prog = node.graph.root.kind match {
      case r @ JavaRoot() => r.program
      case r => throw new Error("root should be of kind JavaRoot instead of " + r)
    }

    decl.setID(node.name)
    decl.setModifiers(new AST.Modifiers("public"))
    val cu = new AST.CompilationUnit()
    cu.setRelativeName(node.name)

    val cpath = node.containerPath
    if(! cpath.head.isRoot)
      throw new AGError("cannot create decl for unrooted node")

    println("setting pathname with " + cpath)
    cu.setPathName(cpath.tail.mkString(java.io.File.separator))
    cu.setTypeDecl(decl, 0)
    cu.setFromSource(true)
    prog.addCompilationUnit(cu)
  }
}


case class Interface private[javaAG]() extends TypeKind {



  def isMergingCandidate(itc : Interface): Boolean ={

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
            otherItc.content.filterNot{hasMatchingMethodIn(this)}*/
        })
  }

  override val toString = "Interface"

  def create() = Interface()

  override def createDecl( n : AGNode[JavaNodeKind]){
    assert(n.kind eq this)
    if(decl == null){
      decl = new AST.InterfaceDecl()

      addDeclToProgram()
    }
  }

  var decl : AST.InterfaceDecl = _

  def canContain(k : JavaNodeKind) : Boolean = {
    k match {
      case AbstractMethod() => true
      case _ => false
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(JavaNodeKind.interface)
    case DelegationAbstraction() => List(JavaNodeKind.`class`)//also interface ?
  }
}

case class Class private[javaAG]() extends TypeKind {

  override val toString = "Class"

  def create() = Class()

  var decl : AST.ClassDecl = _

  override def createDecl( n : AGNode[JavaNodeKind]){
    assert(n.kind eq this)
    if(decl == null){
      decl = new AST.ClassDecl()

      addDeclToProgram()
    }
  }

  def canContain(k : JavaNodeKind) : Boolean = {
    k match {
      case Constructor()
           | Field()
           | Method() => true
      case _ => false
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(JavaNodeKind.interface, JavaNodeKind.`class`)
    case DelegationAbstraction() => List(JavaNodeKind.`class`)//also interface ?
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