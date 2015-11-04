package puck
package intellij

import java.io.{FileReader, File}

import com.intellij.openapi.module.Module
import com.intellij.psi.JavaPsiFacade
import puck.graph._
import puck.graph.constraints.ConstraintsParser
import puck.graph.io.DG2AST
import puck.graph.transformations._
import puck.intellij.concretize.{CreateEdge, CreateNode}
import puck.javaGraph.nodeKind.Definition
import puck.util.{PuckLog, PuckLogger}
import puck.graph.ShowDG._

class IntellijDG2AST
( val module : Module,
  val initialGraph : DependencyGraph,
  val initialRecord : Seq[Transformation],
  val nodesByName : Map[String, NodeId],
  val graph2ASTMap : Map[Int, PsiNodeWrapper]) extends DG2AST {

  implicit lazy val factory = JavaPsiFacade.getElementFactory(module.getProject)

  def apply(graph : DependencyGraph)(implicit logger : PuckLogger) : Unit = {
    logger.writeln("applying change !")
    val record = graph.recording

    ignore(record.reverse.foldLeft((graph, initialGraph, graph2ASTMap)) {
      case ((resultGraph, reenactor, id2declMap), t : Transformation) =>

        //logger.writeln("applying " + (reenactor, t).shows)

      val newId2declMap =
          t match {
            case CreateNode(n) =>
              /*if(n.kind == Definition) id2declMap
              else*/ id2declMap get n.id match {
                case Some(_) => id2declMap
                case None => CreateNode(resultGraph, id2declMap, n)
              }
            case _ =>
              applyOneTransformation(resultGraph, reenactor, id2declMap, t)
              id2declMap

          }

        (resultGraph, t.redo(reenactor), newId2declMap)

      case (acc, op) =>
        logger.writeln((acc._1, op).shows)
        acc
    })

  }

  def printCode(dir : File)(implicit logger : PuckLogger) : Unit = {
    //val virDir = LocalFileSystem.getInstance().findFileByIoFile(dir)
  }


  def parseConstraints(decouple : File)(implicit logger : PuckLogger) : DG2AST =
    try {
      //val parser = ConstraintsPlParser(nodesByName)
      val cm = ConstraintsParser(nodesByName, new FileReader(decouple))
      new IntellijDG2AST(module,
        initialGraph.newGraph(constraints = cm),
        initialRecord,
        nodesByName,
        graph2ASTMap)
    } catch {
      case e : Error =>
        //e.printStackTrace()
        logger.writeln("parsing failed : " + e.getMessage)((PuckLog.NoSpecialContext, PuckLog.Error))
        this
    }

  def code(graph : DependencyGraph, id : NodeId) : String = graph2ASTMap get id  match {
    case None => "no code"
    case Some(PackageDummyWrapper) => "package " + graph.fullName(id)
    case Some(hn : HasNode) => hn.node.toString
    case Some(PrimitiveDeclHolder(t)) => t.toString
    case Some(ArrayTypeWrapper) => "ArrayTypeWrapper"
  }

  def applyOneTransformation
  ( resultGraph : DependencyGraph,
    reenactor: DependencyGraph,
    id2declMap: Map[NodeId, PsiNodeWrapper],
    t: Transformation)
  ( implicit logger : PuckLogger): Unit = {
    lazy val noApplyMsg = s"${(resultGraph, t).shows} not applied"
    t match {
      case CreateNode(n) =>
        sys.error("should be filtered before")
      case CreateEdge(e) =>
        CreateEdge(reenactor, id2declMap, e)

      case t1 => logger.writeln(noApplyMsg)


      /*case Transformation(_, RedirectionWithMerge(_, Source(_))) =>
        logger.writeln(noApplyMsg)

      case Transformation(_, RedirectionOp(e, Source(newSource))) =>
        RedirectSource(resultGraph, reenactor, safeGet(resultGraph, id2declMap), e, newSource)

      case Transformation(_, RedirectionOp(e, Target(newTarget))) =>
        RedirectTarget(resultGraph, reenactor, safeGet(resultGraph, id2declMap), e, newTarget)

      case Transformation(_, TypeChange(user, None, Some(NamedType(newType)))) =>
        CreateEdge.createTypeUse(safeGet(resultGraph, id2declMap), user, newType)

      case Transformation(_, TypeChange(user, Some(NamedType(oldType)), Some(NamedType(newType)))) =>
        RedirectTarget.setType(resultGraph, reenactor, safeGet(resultGraph, id2declMap), user, newType)

      // TODO see if can be performed in add node instead
      case Transformation(_, AbstractionOp(impl, AccessAbstraction(abs, SupertypeAbstraction))) =>
        (id2declMap get impl, reenactor.getConcreteNode(abs).kind) match {
          case (Some(ConcreteMethodDeclHolder(decl)), AbstractMethod) =>
            decl.setVisibility(AST.ASTNode.VIS_PUBLIC)
          case _ => ()
        }

      case Transformation(_, AbstractionOp(_, _)) => ()

      case Transformation(Reverse, CNode(n)) =>
        id2declMap get n.id foreach {
          case dh: TypedKindDeclHolder => dh.decl.puckDelete()
          case bdh: HasBodyDecl => bdh.decl.puckDelete()
          case PackageDeclHolder => ()

          case BlockHolder(_)
               | ParameterDeclHolder(_)
               | ExprHolder(_) => () //removed when containing decl is removed
          case NoDecl => throw new PuckError(noApplyMsg)
        }

      case Transformation(_, ChangeNodeName(nid, _, newName)) =>
        ASTNodeLink.setName(newName, safeGet(reenactor, id2declMap)(nid))

      case Transformation(_, ChangeTypeBinding(((tUser, tUsed), tmUse), TypeUse(newTuse@(ntUser, ntUsed))))
        if tUser == tUsed =>
        if (ntUser != ntUsed && !reenactor.isa_*(ntUsed, ntUser) && !reenactor.isa_*(ntUser, ntUsed))
          createVarAccess(reenactor, safeGet(reenactor, id2declMap), tmUse, newTuse,
            replaceSelfRefByVarAccess)

      case Transformation(Regular, TypeDependency(typeUse@(tUser, tUsed), tmUse)) =>
        if (tUser != tUsed && tUser != tmUse.user)
          createVarAccess(reenactor, safeGet(reenactor, id2declMap), tmUse, typeUse,
            introVarAccess)

      case Transformation(_, ChangeTypeBinding(((oldTypeUser, _), (tmUser, tmUsed)), TypeUse((newTypeUser, newTypeUsed))))
        if oldTypeUser != newTypeUser =>
        replaceMessageReceiver(reenactor, safeGet(reenactor, id2declMap), tmUser, tmUsed, oldTypeUser, newTypeUser)

      case Transformation(_, op) =>
        if (discardedOp(op)) ()
        else logger.writeln(noApplyMsg)*/

    }
  }
//  val introVarAccess : (AST.ASTNode[_], AST.MemberDecl, AST.Access) => Unit =
//    (user, decl, access) => user.introduceVarAccess(decl, access)
//
//  val replaceSelfRefByVarAccess : (AST.ASTNode[_], AST.MemberDecl, AST.Access) => Unit =
//    (user, decl, access) => user.replaceThisQualifierFor(decl, access)
//
//
//  def createVarAccess
//  ( reenactor : DependencyGraph,
//    id2declMap: NodeId => ASTNodeLink,
//    typeMemberUse : NodeIdP,
//    typeUse : NodeIdP,
//    f : (AST.ASTNode[_], AST.MemberDecl, AST.Access) => Unit)
//  : Unit = {
//    val v : AST.Variable = id2declMap(typeUse.user) match {
//      case VariableDeclHolder(decl) => decl
//      case dh => error(s"expect parameter or field, $dh not handled")
//    }
//
//    val newAccess = v.createLockedAccess()
//
//    val user : AST.ASTNode[_] = id2declMap(typeMemberUse.user) match {
//      case defh : DefHolder => defh.node
//      case nodeHolder => error("create var access, expect a def " +
//        s"(expr or block) as user but found  $nodeHolder")
//    }
//
//    val (usedAsVisible : AST.Visible, usedAsMemberDecl : AST.MemberDecl) =
//      id2declMap(typeMemberUse.used) match {
//        case FieldDeclHolder(fdecl) => (fdecl, fdecl)
//        case mdh : MethodDeclHolder => (mdh.decl, mdh.decl)
//        case h => error(s"self use of $h by $user case unhandled")
//      }
//
//    f(user, usedAsMemberDecl, newAccess)
//
//
//    ASTNodeLink.enlargeVisibility(
//      reenactor, usedAsVisible,
//      typeMemberUse.used)
//
//  }
//
//
//  def replaceMessageReceiver
//  ( reenactor : DependencyGraph,
//    id2declMap: NodeId => ASTNodeLink,
//    methodUser : NodeId,
//    methodUsed : NodeId,
//    oldMessageReceiver : NodeId,
//    newMessageReceiver : NodeId
//    )
//  ( implicit logger : PuckLogger): Unit = {
//
//    val mUser = id2declMap(methodUser) match {
//      case dh : DefHolder => dh.node
//      case nodeHolder => error("replace message receiver, expect a def " +
//        s"(expr or block) as user but found $nodeHolder")
//    }
//
//    val mUsed : AST.MemberDecl = id2declMap(methodUsed) match {
//      case dh : HasMemberDecl => dh.decl
//      case nodeHolder => error("replace message receiver, expect" +
//        s" a field or a method as used but found $nodeHolder")
//    }
//
//    id2declMap(newMessageReceiver) match {
//      case VariableDeclHolder(newReceiver) =>
//        id2declMap(oldMessageReceiver) match {
//          case VariableDeclHolder(oldReceiver) =>
//            mUser.replaceMessageReceiver(mUsed, oldReceiver, newReceiver)
//          case MethodDeclHolder(oldReceiver) => ???
//          case dh : DefHolder =>
//            mUser.replaceAllMessageReceiver(mUsed, newReceiver)
//          //case ClassDeclHolder(_)=>
//          //  logger.writeln("ignore replaceMessageReceiver with old receiver beeing a class. Suppose merge is ongoing ")
//          case nodeHolder => error("replace message receiver, expect a def " +
//            s"(expr or block), a field or a parameter as old receiver but found $nodeHolder")
//        }
//      case ClassDeclHolder(_)=>
//        logger.writeln("ignore replaceMessageReceiver as new receiver beeing a class. " +
//          "Suppose merge is ongoing ")
//
//      case nodeHolder => error("replace message receiver, expect" +
//        s" a field or a parameter as new receiver but found $nodeHolder")
//    }
// }

}
