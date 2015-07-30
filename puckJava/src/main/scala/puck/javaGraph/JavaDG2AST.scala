package puck.javaGraph

import java.io.{FileReader, File}
import java.util.NoSuchElementException

import puck.PuckError
import puck.graph.constraints.{SupertypeAbstraction, ConstraintsParser}
import puck.graph.transformations._
import puck.graph._
import puck.graph.io.{DG2AST, DG2ASTBuilder}
import puck.javaGraph.concretize.{RedirectTarget, RedirectSource, CreateEdge, CreateNode}
import puck.javaGraph.nodeKind._
import puck.util.PuckLog._
import puck.util.{PuckLog, PuckLogger}
import ShowDG._

object JavaDG2AST extends DG2ASTBuilder {

  def apply
  ( srcDirectory : File,
    outDirectory : File,
    jarListFile : File,
    logger : PuckLogger,
    ll : puck.LoadingListener = null
    ) : DG2AST = {
    import puck.util.FileHelper.{fileLines, findAllFiles}

    val sProg = puck.util.Time.time(logger, defaultVerbosity) {
      logger.writeln("Compiling sources ...")

      val srcSuffix = ".java"
      val sources = findAllFiles(srcDirectory, srcSuffix, outDirectory.getName)
      val jars = findAllFiles(srcDirectory, ".jar", outDirectory.getName)

      CompileHelper(sources, fileLines(jarListFile) ++: jars )
    }

    puck.util.Time.time(logger, defaultVerbosity) {
      logger.writeln("Building Access Graph ...")
      sProg match {
        case None => throw new DGBuildingError("Compilation error, no AST generated")
        case Some(p) =>
          val t = CompileHelper.buildGraph(p, ll)
          new JavaDG2AST(t._1, t._2, t._3, t._4, t._5)
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

class JavaDG2AST
(val program : AST.Program,
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
        else
          NoDecl
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
      new JavaDG2AST(program,
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


  def apply(result : DependencyGraph)(implicit logger : PuckLogger) : Unit = {

    logger.writeln("applying change !")
    val record = recordOfResult(result)

    record.reverse.foldLeft((graphOfResult(result), initialGraph, graph2ASTMap)) {
      case ((resultGraph, reenactor, g2AST), t : Transformation) =>

        val newG2AST = applyOneTransformation(resultGraph, reenactor, g2AST, t)

        (resultGraph, t.redo(reenactor), newG2AST)

      case (acc, op) =>
        logger.writeln((acc._1, op).shows)
        acc
    }

    logger.writeln("change applied : ")
    logger.writeln(program)
    program.flushCaches()
    program.eliminateLockedNamesInSources()
    logger.writeln("Program after unlock : ")
    logger.writeln(program)

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
      if(n.kind == Definition)
        id2declMap
      else {
        val newMap = id2declMap get n.id match {
          case Some(_) => id2declMap
          case None =>
            val dh = CreateNode(program, resultGraph, id2declMap, n)

            id2declMap + (n.id -> dh)
        }
        newMap
      }
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
          case bdh : HasBodyDecl => bdh.decl.puckDelete()
          case PackageDeclHolder => ()

          case BlockHolder(_)
           | ParameterDeclHolder(_)
           | ExprHolder(_) => () //removed when containing decl is removed
          case NoDecl => throw new PuckError(noApplyMsg)
        }

      case Transformation(_, ChangeNodeName(nid, _, newName)) =>
        ASTNodeLink.setName(newName, safeGet(reenactor,id2declMap)(nid))

      case Transformation(_, ChangeTypeBinding(((tUser, tUsed), tmUse), TypeUse(newTuse)))
        if tUser == tUsed =>
        replaceSelfRefByVarAccess(reenactor, safeGet(reenactor,id2declMap), tmUse, newTuse)

      case Transformation(_, op) =>
        if( discardedOp(op) ) ()
        else logger.writeln(noApplyMsg)
    }
    id2declMap
  }




  def replaceSelfRefByVarAccess
  ( reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    typeMemberUse : NodeIdP,
    newTypeUse : NodeIdP) : Unit = {
    val v : AST.Variable = id2declMap(newTypeUse.user) match {
      case ParameterDeclHolder(pdecl) => pdecl
      case FieldDeclHolder(fdecl) => fdecl
      case _ => ???
    }

    val newAccess = v.createLockedAccess()

    val user : AST.ASTNode[_] = id2declMap(typeMemberUse.user) match {
      case defh : DefHolder => defh.node
      case _ => ???
    }
    val used : AST.Visible =
      id2declMap(typeMemberUse.used) match {
      case FieldDeclHolder(fdecl) =>
        user.replaceThisQualifierFor(fdecl, newAccess)
        fdecl
      case mdh : MethodDeclHolder =>
        user.replaceThisQualifierFor(mdh.decl, newAccess)
        mdh.decl
      case _ => ???
    }
    ASTNodeLink.enlargeVisibility(
      reenactor, used,
      typeMemberUse.used)

  }


}