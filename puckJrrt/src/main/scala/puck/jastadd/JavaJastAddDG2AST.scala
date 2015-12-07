package puck.jastadd

import java.io.{File, FileReader}
import java.util.NoSuchElementException

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.{ConstraintsParser, SupertypeAbstraction}
import puck.graph.io.{DG2AST, DG2ASTBuilder}
import puck.graph.transformations._
import puck.jastadd.concretize._
import puck.javaGraph.nodeKind._
import puck.util.PuckLog._
import puck.util.{PuckLog, PuckLogger}
import org.extendj.{ast => AST}

object JavaJastAddDG2AST extends DG2ASTBuilder {

  def apply
  ( srcDirectory : File,
    outDirectory : Option[File],
    jarListFile : Option[File],
    logger : PuckLogger,
    ll : puck.LoadingListener = null
    ) : DG2AST = {
    import puck.util.FileHelper.{fileLines, findAllFiles}

    val sProg = puck.util.Time.time(logger, defaultVerbosity) {
      logger.writeln("Compiling sources ...")

      val srcSuffix = ".java"
      val sources = findAllFiles(srcDirectory, srcSuffix, outDirectory map (_.getName))
      val jars = findAllFiles(srcDirectory, ".jar", outDirectory map (_.getName))
      val jarsFromList =
        jarListFile map (fileLines(_, keepEmptyLines = false)) getOrElse List()
      CompileHelper(sources, jarsFromList ++: jars )
    }

    puck.util.Time.time(logger, defaultVerbosity) {
      logger.writeln("Building Access Graph ...")
      sProg match {
        case None => throw new DGBuildingError("Compilation error, no AST generated")
        case Some(p) =>
          val t = CompileHelper.buildGraph(p, ll)
          new JavaJastAddDG2AST(t._1, t._2, t._3, t._4, t._5)
        }
    }

    /*val (numClass, numItc) = g.concreteNodes.foldLeft((0,0)){ case ((numClass0, numItc0), n) =>
      val numClass1 = if(n.kind == nodeKind.Class) numClass0 + 1
      else numClass0
      val numItc1 = if(n.kind == nodeKind.Interface) numItc0 + 1
      else numItc0
      (numClass1, numItc1)

    }
    logger.writeln( numClass + " classes and " + numItc + " interfaces parsed")
*/
  }
  def verbosity : PuckLog.Level => PuckLog.Verbosity = l => (PuckLog.AG2AST, l)
}

class JavaJastAddDG2AST
( val program : AST.Program,
  val initialGraph : DependencyGraph,
  val initialRecord : Seq[Transformation],
  val nodesByName : Map[String, NodeId],
  val graph2ASTMap : Map[NodeId, ASTNodeLink]) extends DG2AST {

  implicit val p = program

  implicit val defaultVerbosity = (PuckLog.AG2AST, PuckLog.Info)

  def safeGet
  ( graph : DependencyGraph,
    id2declMap : Map[NodeId, ASTNodeLink] )
  ( id : NodeId ) : ASTNodeLink =
    try id2declMap(id)
    catch {
      case e : NoSuchElementException =>
        val n = graph.getNode(id)
        if(n.kind == Package)
          PackageDeclHolder
        else NoDecl
    }

  def astNodeOf(graph : DependencyGraph, id : NodeId) : ASTNodeLink =
    safeGet(graph, graph2ASTMap)(id)

  def code(graph : DependencyGraph, id : NodeId) : String =
    astNodeOf(graph,id).toString



  def parseConstraints
  ( decouple : File)
  ( implicit logger : PuckLogger) : DG2AST  =
    try {
      //val parser = ConstraintsPlParser(nodesByName)
      val cm = ConstraintsParser(nodesByName, new FileReader(decouple))
      new JavaJastAddDG2AST(program,
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


  def apply(graph : DependencyGraph)(implicit logger : PuckLogger) : Unit = {

//    def printCUs() = {
//      println(program.getNumCompilationUnit.toString + "c us")
//      List.range(0, program.getNumCompilationUnit).foreach {
//        i =>
//          val cu = program.getCompilationUnit(i)
//          if(cu.fromSource()) {
//            println(cu.pathName() + " " + cu.getPackageDecl)
//            List.range(0, cu.getNumTypeDecl).foreach{
//              j =>
//                println(cu.getTypeDecl(j).fullName())
//            }
//          }
//      }
//    }

    logger.writeln("applying change !")
    val record = graph.recording

    logger.writeln("before applying change : ")
    logger.writeln(program.prettyPrint())

    record.reverse.foldLeft((graph, initialGraph, graph2ASTMap)) {
      case ((resultGraph, reenactor, g2AST), t : Transformation) =>

        logger.writeln("applying " + (reenactor, t).shows)
        val newG2AST = applyOneTransformation(resultGraph, reenactor, g2AST, t)

        (resultGraph, t.redo(reenactor), newG2AST)

      case (acc, op) =>
        logger.writeln((acc._1, op).shows)
        acc
    }

    logger.writeln("change applied : ")
    logger.writeln(program.prettyPrint())
    program.flushCaches()

    try
      program.eliminateLockedNamesInSources()
    catch {
      case e : Exception =>
        e.printStackTrace()
    }
//    program.eliminateFreshVariables()

    logger.writeln("Program after unlock : ")
    logger.writeln(program.prettyPrint())

  }

  def printCode(dir : File)(implicit logger : PuckLogger) : Unit =
    program.printCodeInDirectory(dir)



  val discardedOp : Operation => Boolean = {
    case _ : Comment
    | _ : TypeDependency => true
    case _ => false

  }

  def applyOneTransformation
  ( resultGraph : DependencyGraph,
    reenactor: DependencyGraph,
    id2declMap: Map[NodeId, ASTNodeLink],
    t: Transformation)
  ( implicit logger : PuckLogger): Map[NodeId, ASTNodeLink] = t match {
    case Transformation(Regular, CNode(n)) =>
      //redo t before createDecl
      if(n.kind == Definition) id2declMap
      else {
        val newMap = id2declMap get n.id match {
          case Some(_) => id2declMap
          case None => CreateNode(program, resultGraph, id2declMap, n)
        }
        newMap
      }
    case Transformation(Regular, Edge(ContainsDef(source, target))) =>
      CreateNode.addDef(program,resultGraph,id2declMap, source, target)

    case _ =>
      lazy val noApplyMsg = s"${(resultGraph, t).shows} not applied"

      t match {
      case Transformation(Regular, Edge(e)) =>
        //println("creating edge " + e)
        CreateEdge(resultGraph, reenactor, safeGet(resultGraph, id2declMap), e)

      case Transformation(_, RedirectionWithMerge(_, Source(_))) =>
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
          case (Some(MethodDeclHolder(decl)), AbstractMethod) =>
            decl.setVisibility(AST.ASTNode.VIS_PUBLIC)
          case _ => ()
        }

      case Transformation(_, AbstractionOp(_, _)) => ()

      case Transformation(Reverse, CNode(n)) =>
        id2declMap get n.id foreach {
          case dh: TypedKindDeclHolder => dh.decl.puckDelete()
          case bdh : HasBodyDecl => bdh.decl.puckDelete()
          case PackageDeclHolder => ()

          case BlockHolder(_)
           | ParameterDeclHolder(_)
           | ExprHolder(_) => () //removed when containing decl is removed
          case NoDecl => throw new PuckError(noApplyMsg)
        }

      case Transformation(_, ChangeNodeName(nid, _, newName)) =>
        ASTNodeLink.setName(newName, safeGet(reenactor,id2declMap)(nid), reenactor, nid)

      case Transformation(_, ChangeTypeBinding(((tUser, tUsed), tmUse), TypeUse(newTuse @ (ntUser, ntUsed))))
        if tUser == tUsed =>
        if (ntUser != ntUsed && !reenactor.isa_*(ntUsed, ntUser) && !reenactor.isa_*(ntUser, ntUsed))
          createVarAccess(reenactor, safeGet(reenactor,id2declMap), tmUse, newTuse,
            replaceSelfRefByVarAccess)

      case Transformation(Regular, TypeDependency(typeUse @(tUser, tUsed), tmUse)) =>
        if (tUser != tUsed && tUser != tmUse.user)
        createVarAccess(reenactor, safeGet(reenactor,id2declMap), tmUse, typeUse,
          introVarAccess)

      case Transformation(_, ChangeTypeBinding(((oldTypeUser, _),(tmUser, tmUsed)), TypeUse((newTypeUser, newTypeUsed))))
        if oldTypeUser != newTypeUser =>
          replaceMessageReceiver(reenactor, safeGet(reenactor, id2declMap), tmUser, tmUsed, oldTypeUser, newTypeUser)

      case Transformation(_, op) =>
        if( discardedOp(op) ) ()
        else logger.writeln(noApplyMsg)
    }
    id2declMap
  }

  val introVarAccess : (AST.ASTNode[_], AST.MemberDecl, AST.Access) => Unit =
    (user, decl, access) => user.introduceVarAccess(decl, access)

  val replaceSelfRefByVarAccess : (AST.ASTNode[_], AST.MemberDecl, AST.Access) => Unit =
    (user, decl, access) => user.replaceThisQualifierFor(decl, access)


  def createVarAccess
  ( reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    typeMemberUse : NodeIdP,
    typeUse : NodeIdP,
    f : (AST.ASTNode[_], AST.MemberDecl, AST.Access) => Unit)
  : Unit = {
    val v : AST.Variable = id2declMap(typeUse.user) match {
      case VariableDeclHolder(decl) => decl
      case dh => error(s"expect parameter or field, $dh not handled")
    }

    val newAccess = v.createLockedAccess()

    val user : AST.ASTNode[_] = id2declMap(typeMemberUse.user) match {
      case defh : DefHolder => defh.node
      case nodeHolder => error("create var access, expect a def " +
        s"(expr or block) as user but found  $nodeHolder")
    }

    val (usedAsVisible : AST.Visible, usedAsMemberDecl : AST.MemberDecl) =
      id2declMap(typeMemberUse.used) match {
        case FieldDeclHolder(fdecl) => (fdecl, fdecl)
        case mdh : MethodDeclHolder => (mdh.decl, mdh.decl)
        case h => error(s"self use of $h by $user case unhandled")
      }

    f(user, usedAsMemberDecl, newAccess)


    ASTNodeLink.enlargeVisibility(
      reenactor, usedAsVisible,
      typeMemberUse.used)

  }


  def replaceMessageReceiver
  ( reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    methodUser : NodeId,
    methodUsed : NodeId,
    oldMessageReceiver : NodeId,
    newMessageReceiver : NodeId
    )
  ( implicit logger : PuckLogger): Unit = {

    val mUser = id2declMap(methodUser) match {
      case dh : DefHolder => dh.node
      case nodeHolder => error("replace message receiver, expect a def " +
        s"(expr or block) as user but found $nodeHolder")
    }

    val mUsed : AST.MemberDecl = id2declMap(methodUsed) match {
      case dh : HasMemberDecl => dh.decl
      case nodeHolder => error("replace message receiver, expect" +
        s" a field or a method as used but found $nodeHolder")
    }

   id2declMap(newMessageReceiver) match {
      case VariableDeclHolder(newReceiver) =>
        id2declMap(oldMessageReceiver) match {
          case VariableDeclHolder(oldReceiver) =>
            mUser.replaceMessageReceiver(mUsed, oldReceiver, newReceiver)
          case MethodDeclHolder(oldReceiver) => ???
          case dh : DefHolder =>
            mUser.replaceAllMessageReceiver(mUsed, newReceiver)
          //case ClassDeclHolder(_)=>
          //  logger.writeln("ignore replaceMessageReceiver with old receiver beeing a class. Suppose merge is ongoing ")
          case nodeHolder => error("replace message receiver, expect a def " +
            s"(expr or block), a field or a parameter as old receiver but found $nodeHolder")
        }
      case ClassDeclHolder(_)=>
         logger.writeln("ignore replaceMessageReceiver as new receiver beeing a class. " +
           "Suppose merge is ongoing ")

      case nodeHolder => error("replace message receiver, expect" +
        s" a field or a parameter as new receiver but found $nodeHolder")
    }



  }


}