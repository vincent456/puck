package puck.javaAG.immutable.nodeKind

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{AGNode, AccessGraph, HasType}
import puck.javaAG.immutable.MethodType
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.AGError

/**
 * Created by lorilan on 31/07/14.
 */
abstract class MethodKind extends JavaNodeKind with HasType[JavaNodeKind, MethodType.T] {

  def decl : Option[AST.MethodDecl]

  def canContain(k : JavaNodeKind) = false

}

case class Method private[javaAG](node : NodeId[JavaNodeKind],
                                  typ : MethodType.T,
                                  decl : Option[AST.MethodDecl]) extends MethodKind {

  override val toString = "Method"

  def create(nodeId : NodeId[JavaNodeKind]) = Method(node, typ, decl)

  override def createDecl(prog : AST.Program,
                          graph : AccessGraph[JavaNodeKind]) : AccessGraph[JavaNodeKind] = {

    decl match {
      case None =>
        val n = graph.getNode(node)

        val someKtor = graph.getNode(n.container).content.find{ n0 =>
          val n1 = graph.getNode(n0)
          n1.kind.isInstanceOf[Constructor] &&
            n1.abstractions.exists {
              case (n2 , DelegationAbstraction()) => n2 == node
              case _ => false
            }
        }

        someKtor match {
          case None =>
            super.createDecl(prog, graph)
          case Some(c) =>
            val ktorKind = graph.getNode(c).kind.asInstanceOf[Constructor]
            val decl = ktorKind.decl.get.createConstructorMethod(n.name)
            graph.setKind(node, Method(node, typ, Some(decl))).graph
        }

      case Some(_) => graph
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(JavaNodeKind.abstractMethod(typ), JavaNodeKind.method(typ))
    case DelegationAbstraction() => List(JavaNodeKind.method(typ))//also abstractMethod ?
  }
}

class ConstructorMethod(node : NodeId[JavaNodeKind],
                        typ : MethodType.T,
                        decl : Option[AST.MethodDecl],
                        val  ctorDecl : Option[AST.ConstructorDecl])
  extends Method(node, typ, decl) {

  override def create(nodeId : NodeId[JavaNodeKind]) =
    new ConstructorMethod(nodeId, typ, None, None)

  override def createDecl(prog : AST.Program,
                          graph : AccessGraph[JavaNodeKind]) : AccessGraph[JavaNodeKind] = {
    decl match {
      case None =>
        val n = graph.getNode(node)
        val ctorMth = new ConstructorMethod(node, typ,
              Some(ctorDecl.get.createConstructorMethod(n.name)), ctorDecl)
        graph.setKind(node, ctorMth).graph
      case Some(_) => graph
    }
  }
}


case class AbstractMethod private[javaAG](node : NodeId[JavaNodeKind],
                                          typ : MethodType.T,
                                          decl : Option[AST.MethodDecl]) extends  MethodKind {

  override val toString = "AbstractMethod"

  def create(nodeId : NodeId[JavaNodeKind]) = new AbstractMethod(node, typ, None)

  override def createDecl(prog : AST.Program,
                          graph : AccessGraph[JavaNodeKind]) : AccessGraph[JavaNodeKind] = {

    (decl, typ) match {
      case (None, mt : MethodType) =>
        val decl = AST.MethodDecl.createAbstractMethod(mt.createReturnAccess(graph),
          graph.getNode(node).name, mt.createASTParamList(graph).toArray)
        val absMth = AbstractMethod(node, typ, Some(decl))
        graph.setKind(node, absMth).graph
      case (None, _) => throw new AGError(" not a method type !!")
      case (Some(_),_) => graph
    }
  }

  def abstractKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction() => List(JavaNodeKind.abstractMethod(typ))
    case DelegationAbstraction() => List(JavaNodeKind.method(typ))//also abstractMethod ?
  }

  def findMergingCandidate(interface : AGNode[JavaNodeKind]) = ??? /*{
    //node.graph.logger.writeln("searching merging candidate for %s".format(node), 8)
    val mType = typ.copyWith(node.container).replacedBy(interface)

    interface.content.find { nc =>
      nc.kind match {
        case ncKind @ AbstractMethod() =>
      //    node.graph.logger.write("trying %s : ".format(nc), 8)
          val isMergingCandidate = nc.name == node.name &&
            ncKind.typ == mType
      //    node.graph.logger.writeln(isMergingCandidate.toString, 8)
          isMergingCandidate
        case _ => false
      }
    }

  }*/
}