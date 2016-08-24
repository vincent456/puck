package puck

import java.io.{File, FileWriter}

import org.extendj.ast.JavaJastAddDG2AST
import puck.graph.{DependencyGraph, MutabilitySet, NodeId, NodeIdP, NodeIdPOps, ShowDG}
import puck.graph.constraints.{ConstraintMapBuilder, ConstraintsMaps, LiteralRangeSet, Scope}
import puck.jastadd.JavaProject
import puck.util.{PuckFileLogger, PuckLogger}
import puck.javaGraph.nodeKind._

import scala.util.Random

/**
  * Created by lorilan on 7/28/16.
  */
object ConstraintGen {


  def candidates(g : DependencyGraph, use : NodeIdP,
                 nodeFilter : (DependencyGraph,NodeId) => Boolean,
                 pairFilter : (DependencyGraph,NodeId, NodeId) => Boolean) = {

    def candidatesInContainers( n : NodeId): Seq[NodeId] = {
      val containers = g.containerPath(n).tail// tail to exclude root
      Random.shuffle(containers filter (nodeFilter(g, _)))
    }
    for{
      hidden <- candidatesInContainers(use.used)
      interloper <-candidatesInContainers(use.user)
      if pairFilter(g, hidden, interloper)
    } yield (hidden, interloper)
  }

  def typeCandidatesInDifferentPackages(g : DependencyGraph, use : NodeIdP ) = {
    def candidatesFilter(g : DependencyGraph, n : NodeId) =
      g.getNode(n).kind match {
        case Interface | Class => true
        case _ => false
      }
    def rightPairCandidate(g : DependencyGraph, n : NodeId, n2 : NodeId) : Boolean = {
        val nCter = g.container_!(n)
        val n2Cter = g.container_!(n2)
        n != n2 && ! g.contains_*(nCter, n2Cter) && ! g.contains_*(n2Cter, nCter)
      }

    candidates(g, use, candidatesFilter, rightPairCandidate)
  }


  def packageOrTypeCandidates(g : DependencyGraph, use : NodeIdP ) = {
    def candidatesFilter(g : DependencyGraph, n : NodeId) =
      g.getNode(n).kind match {
        case Package  | Interface | Class => true
        case _ => false
      }

    def rightPairCandidate(g : DependencyGraph, n : NodeId, n2 : NodeId) : Boolean =
      ! g.contains_*(n, n2) && ! g.contains_*(n2, n)

    candidates(g, use, candidatesFilter, rightPairCandidate)
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

        val newBuilder = builder.addHideConstraint(LiteralRangeSet(Scope(hidden)),
          LiteralRangeSet(),
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
    val baseName = s"constraint-gen$numConstraint-07"

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
    val builder = genConstraints(g, typeCandidatesInDifferentPackages,
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

    logger writeln ((g,cm).violations.size + " violations")
    (g, dg2ast.nodesByName, cm, dg2ast.initialMutability)
  }
}
