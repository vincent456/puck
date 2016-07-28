package puck

import java.io.FileWriter

import org.extendj.ast.JavaJastAddDG2AST
import puck.graph.{DGNode, DependencyGraph, NameSpace, NodeId, NodeIdP, ShowDG, TypeDecl}
import puck.graph.constraints.{ConstraintMapBuilder, ConstraintsMaps, LiteralRangeSet, Scope}
import puck.jastadd.JavaProject
import puck.util.PuckSystemLogger

import scala.util.Random
/**
  * Created by lorilan on 7/28/16.
  */
object ConstraintGen {

  def main (args: Array[String]) : Unit = {

    implicit val logger = new PuckSystemLogger(_ => true)

    val root = "/home/lorilan/projects/arianne-marauroa"

    val projectFileName = root + "/original-puck-cfg.xml"

    val decoupleName = "constraint-gen3-01.wld"
    val numConstraint = 1

    val p = JavaProject.withConfig(projectFileName)

    val dg2ast: JavaJastAddDG2AST = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]


    def candidatesFilter(g : DependencyGraph, n : NodeId) =
      g.kindType(n) match {
        case TypeDecl | NameSpace => true
        case _ => false
      }

    val g = dg2ast.initialGraph

    val usesList = Random.shuffle(g.usesList)

    import puck.graph.ShowDG._
    def createRandomConstraint
    ( usesList : List[NodeIdP],
      numConstraint : Int,
      builder : ConstraintMapBuilder) : ConstraintMapBuilder =
      if(usesList.isEmpty || numConstraint <=0) builder
      else {
        val (user, used) = usesList.head

        def candidatesInContainers( n : NodeId): Seq[NodeId] = {
          val containers = g.containerPath(n).tail// tail to exclude root

          Random.shuffle(containers.tail filter (candidatesFilter(g, _)))
        }

        def rightPairCandidate(n : NodeId, n2 : NodeId) : Boolean =
          ! g.contains_*(n, n2) && ! g.contains_*(n2, n)


        val candidates = for{
          hidden <- candidatesInContainers(used)
          candidate <-candidatesInContainers(user)
          if rightPairCandidate(hidden, candidate)
        } yield (hidden, candidate)



        if(candidates.nonEmpty) {
          val (hidden, interloper)= candidates.head
          println(s"hide ${(g, hidden).shows(desambiguatedFullName)} " +
            s"from ${(g, interloper).shows(desambiguatedFullName)}")

          val newBuilder = builder.addHideConstraint(LiteralRangeSet(Scope(hidden)),
            LiteralRangeSet(),
            LiteralRangeSet(Scope(interloper)),
            LiteralRangeSet())
          createRandomConstraint(usesList.tail, numConstraint - 1, newBuilder)
        }
        else
          createRandomConstraint(usesList.tail, numConstraint, builder)

      }

    println(usesList.size + " uses")
    val builder = createRandomConstraint(usesList, numConstraint, ConstraintMapBuilder(dg2ast.nodesByName))

    val cm = ConstraintsMaps(builder.defs,
      builder.friendCtsMap,
      builder.hideConstraintsMap)

    println("printing constraints ...")
    import puck.util.FileHelper.FileOps

    val fw = new FileWriter(p.workspace \ decoupleName)
    import ShowDG._
    fw write (g,cm).shows
    fw.close()

    println((g,cm).violations.size + " violations")
  }
}
