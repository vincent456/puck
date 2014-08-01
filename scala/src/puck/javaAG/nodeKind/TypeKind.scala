package puck.javaAG.nodeKind

import puck.graph.{AGError, AGNode}
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.javaAG.MethodType

/**
 * Created by lorilan on 31/07/14.
 */
abstract class TypeKind extends JavaNodeKind {
  def decl : AST.TypeDecl
  def createLockedAccess() : AST.Access = decl.createLockedAccess()
  def addDeclToProgram(){
    val prog = node.graph.root.kind match {
      case r @ JavaRoot() => r.program
      case r => throw new Error("root shoold be of kind JavaRoot instead of " + r)
    }

    decl.setID(node.name)
    decl.setModifiers(new AST.Modifiers("public"))
    val cu = new AST.CompilationUnit()
    cu.setTypeDecl(decl, 0)
    cu.setFromSource(true)
    prog.addCompilationUnit(cu)
  }
}


case class Interface private[javaAG]() extends TypeKind {

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