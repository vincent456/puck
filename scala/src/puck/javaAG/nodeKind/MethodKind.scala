package puck.javaAG.nodeKind

import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.{AGNode, HasType}
import puck.javaAG.MethodType

/**
 * Created by lorilan on 31/07/14.
 */
abstract class MethodKind extends JavaNodeKind with HasType[MethodType] {

  var decl : AST.MethodDecl = _

  def canContain(k : JavaNodeKind) = false

  override def redirectUses(oldUsee : AGNode[JavaNodeKind],
                            newUsee : AGNode[JavaNodeKind]){

    `type` = `type`.copyWith(oldUsee).replacedBy(newUsee)

  }

}

case class Method private[javaAG]() extends MethodKind {

  override val toString = "Method"

  def create() = JavaNodeKind.method(`type`)

  /*override def createDecl(n : AGNode[JavaNodeKind]) = {
    assert(n.kind eq this)
    n.container.content.find{ n1 =>
        n1.kind == Constructor() &&
        n1.abstractions.exists{
          case (n2 , DelegationAbstraction()) => n2 == n
          case _ => false
        }
    } match {
      case None =>
        super.createDecl(n)
      case Some(c) =>
        decl = c.kind.asInstanceOf[Constructor].decl.createConstructorMethod(n.name)
        decl
    }
  }*/

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(JavaNodeKind.abstractMethod(`type`), JavaNodeKind.method(`type`))
    case DelegationAbstraction() => List(JavaNodeKind.method(`type`))//also abstractMethod ?
  }
}

class ConstructorMethod extends Method {

  var ctorDecl : AST.ConstructorDecl = _

  override def create() = { val nc = new ConstructorMethod()
    nc.`type`= this.`type`
    nc
  }

  override def createDecl(n : AGNode[JavaNodeKind]) = {
    assert(n.kind eq this)
    decl = ctorDecl.createConstructorMethod(n.name)
  }
}

case class AbstractMethod private[javaAG]() extends  MethodKind {

  override val toString = "AbstractMethod"

  def create() = JavaNodeKind.abstractMethod(`type`)

  override def createDecl( n : AGNode[JavaNodeKind]) = {
    assert(n.kind eq this)
    if(decl == null){
      decl = AST.MethodDecl.createAbstractMethod(`type`.createReturnAccess(),
        n.name, `type`.createASTParamList().toArray)
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(JavaNodeKind.abstractMethod(`type`))
    case DelegationAbstraction() => List(JavaNodeKind.method(`type`))//also abstractMethod ?
  }

}