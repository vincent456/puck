package puck

import java.io.{File, FileWriter}

import org.extendj.ast.JavaJastAddDG2AST
import org.extendj.JavaProject
import puck.graph.{DependencyGraph, MutabilitySet, NameSpace, NodeId, NodeIdP, NodeIdPOps, ShowDG, TypeDecl}
import puck.graph.constraints.{ConstraintMapBuilder, ConstraintsMaps, LiteralRangeSet, Scope}
import puck.util.{PuckFileLogger, PuckLogger}
import puck.javaGraph.nodeKind._

import scala.util.Random

/**
  * Created by lorilan on 7/28/16.
  */
object ConstraintGen {


  def candidates(g : DependencyGraph, use : NodeIdP,
                 nodeFilter : (DependencyGraph,NodeId) => Boolean,
                 pairFilter : (DependencyGraph,NodeId, NodeId) => Boolean) : Seq[NodeIdP] = {

    def candidatesInContainers( n : NodeId): Seq[NodeId] = {
      val containers = g.containerPath(n).tail // tail to exclude root
      Random.shuffle(containers filter (nodeFilter(g, _)))
    }
    def noInterloperIsaHidden(hidden : NodeId, interloper: NodeId) : Boolean =
    {
      val hs = g.subTree(hidden).withFilter(h => g.kindType(h) == TypeDecl)
      (for {
        i <- g.subTree(interloper)
        if g.kindType(i) == TypeDecl
        h <- hs
      } yield (i, h)).forall { case (i, h) => ! g.isa_*(i, h) }
    }
    for{
      hidden <- candidatesInContainers(use.used)
      interloper <-candidatesInContainers(use.user)
      if pairFilter(g, hidden, interloper) && noInterloperIsaHidden(hidden, interloper)
    } yield (hidden, interloper)
  }


  def oneNodeDoesNotContainsAnother(g : DependencyGraph, n : NodeId, n2 : NodeId) : Boolean =
    ! g.contains_*(n, n2) && ! g.contains_*(n2, n)

  def nodeInDifferentPackages(g : DependencyGraph, n : NodeId, n2 : NodeId) : Boolean = {
    val Some(nCter) = g.containerOfKindType(NameSpace, n)
    val Some(n2Cter) = g.containerOfKindType(NameSpace,n2)
    n != n2 && ! g.contains_*(nCter, n2Cter) && ! g.contains_*(n2Cter, nCter)
  }

  def typeCandidatesInDifferentPackages
  ( ignoredPackages : Seq[NodeId])
  ( g : DependencyGraph, use : NodeIdP ) : Seq[NodeIdP] = {
    def candidatesFilter(g : DependencyGraph, n : NodeId) =
      g.getNode(n).kind match {
        case Interface | Class => !ignoredPackages.exists(g.contains_*(_, n))
        case _ => false
      }

    candidates(g, use, candidatesFilter, nodeInDifferentPackages)
  }

  def typeCandidatesInDifferentPackages(g : DependencyGraph, use : NodeIdP ) = {
    def candidatesFilter(g : DependencyGraph, n : NodeId) =
      g.getNode(n).kind match {
        case Interface | Class => true
        case _ => false
      }


    candidates(g, use, candidatesFilter, nodeInDifferentPackages)
  }


  def packageOrTypeCandidates(g : DependencyGraph, use : NodeIdP ) = {
    def candidatesFilter(g : DependencyGraph, n : NodeId) =
      g.getNode(n).kind match {
        case Package  | Interface | Class => true
        case _ => false
      }


    candidates(g, use, candidatesFilter, oneNodeDoesNotContainsAnother)
  }





  import puck.graph.ShowDG._
  def genConstraints
  ( g : DependencyGraph,
    genCandidates : (DependencyGraph, NodeIdP) => Seq[NodeIdP],
    usesList : List[NodeIdP],
    numConstraint : Int,
    builder : ConstraintMapBuilder) : ConstraintMapBuilder = {


    def aux(usesList : List[NodeIdP],
            numConstraint : Int,
            builder : ConstraintMapBuilder) : ConstraintMapBuilder =
    if(usesList.isEmpty || numConstraint <= 0) builder
    else {
      val candidates = genCandidates(g, usesList.head)

      if (candidates.nonEmpty) {
        val (hidden, interloper) = candidates.head
        println(s"hide ${(g, hidden).shows(desambiguatedFullName)} " +
          s"from ${(g, interloper).shows(desambiguatedFullName)}")

        //do not hide enums
        val facades = g.subTree(hidden, includeRoot = false).filter {
          id => g.getConcreteNode(id).kind == Enum
        } map Scope.apply

        val newBuilder = builder.addHideConstraint(LiteralRangeSet(Scope(hidden)),
          LiteralRangeSet(facades),
          LiteralRangeSet(Scope(interloper)),
          LiteralRangeSet())
        aux(usesList.tail, numConstraint - 1, newBuilder)
      }
      else
        aux(usesList.tail, numConstraint, builder)
    }
    aux(usesList, numConstraint, builder)
  }

  def main (args: Array[String]) : Unit = {

    val root = "/home/lorilan/projects/arianne-marauroa"

    val projectFileName = root + "/original-puck-cfg.xml"

    val numConstraint = 1
    val baseName = s"constraint-gen$numConstraint-11"

    implicit val logger = new PuckFileLogger(_ => true,
      new File(root + File.separator + baseName + ".log"))

    val p = JavaProject.withConfig(projectFileName)
    puck.ignore(this.apply(p, baseName, numConstraint))
  }


  def apply(p : Project, baseName : String, numConstraint : Int)
      ( implicit logger : PuckLogger) :
      (DependencyGraph, Map[String, NodeId], ConstraintsMaps, MutabilitySet) = {

    val dg2ast: JavaJastAddDG2AST = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]

    val g = dg2ast.initialGraph

    val usesList = Random.shuffle(g.usesList)

    logger writeln (usesList.size + " uses")

    val Some(javaPkg) = DependencyGraph.findElementByName(g, "java")
    val Some(primPkg) = DependencyGraph.findElementByName(g, "@primitive")

    val libraryPkgs = dg2ast.fromLibrary.toSeq filter (g.kindType(_) == NameSpace)
    val genCandidates = typeCandidatesInDifferentPackages(
      javaPkg.id +: primPkg.id +: libraryPkgs) _

    val builder = genConstraints(g, genCandidates,
      usesList, numConstraint, ConstraintMapBuilder(dg2ast.nodesByName))

    val cm = ConstraintsMaps(builder.defs,
      builder.friendCtsMap,
      builder.hideConstraintsMap)

    logger writeln "printing constraints ..."
    import puck.util.FileHelper.FileOps

    val fw = new FileWriter(p.workspace \ baseName + ".wld")
    import ShowDG._
    fw write (g,cm).shows
    fw.close()

    logger writeln ((cm forbiddenDependencies g).size + " violations")
    (g, dg2ast.nodesByName, cm, dg2ast.initialMutability)
  }
}
