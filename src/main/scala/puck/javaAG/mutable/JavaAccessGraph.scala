package puck.javaAG.mutable

import puck.graph.constraints.RedirectionPolicy
import puck.graph.mutable.backTrack.Recording
import puck.graph.mutable.{AGEdge, AGNode, AccessGraph}
import puck.javaAG.mutable.nodeKind._
import puck.util.PuckLog

import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.mutable.StringBuilder

/**
 * Created by lorilan on 07/05/14.
 */

object JavaAccessGraph {

  val defaultPackageName = "<default package>"

  def filterPackageName(name: String) = name match {
    case "" => defaultPackageName
    case _ => name
  }
}

class JavaAccessGraph extends AccessGraph[JavaNodeKind](JavaNode){



  override def newGraph() : JavaAccessGraph ={
    new JavaAccessGraph()
  }


  def program = root.kind match {
    case r @ JavaRoot() => r.program
    case _ => throw new Error("root.kind must be JavaRoot")
  }

  /*def doMerges(){

    class MergeDone extends Throwable

    try {
      this.foreach { n =>
        n.kind match {
          case Interface() =>
            n.findMergingCandidate() match {
              case Some(other) =>
                if(n.users.forall{!_.interloperOf(other)}) {
                  n.mergeWith(other)
                  throw new MergeDone()
                }
                else if(other.users.forall{!_.interloperOf(n)}){
                  other.mergeWith(n)
                  throw new MergeDone()
                }
                //else do nothing
              case None => ()
            }

          case _ => ()
        }
      }
    } catch {
      case e : MergeDone => doMerges()
    }

  }*/

  override def applyChangeOnProgram(record : Recording[JavaNodeKind]){

    transformations.recording.undo()

    logger.writeln("applying change !")

    record.foreach { r =>
      AG2AST(r)
      r.redo()
    }
    program.flushCaches()
    program.eliminateLockedNames()
  }

  override def coupling = this.foldLeft(0 : Double){ (acc, n) => n.kind match {
    case Package() =>
      val c = n.coupling
      if(c.isNaN) acc
      else acc + c
    case _ => acc
  }}

  override def redirectUses(oldUse : EdgeType, newUsee : NodeType,
                            policy : RedirectionPolicy,
                            propagateRedirection : Boolean = true,
                            keepOldUse : Boolean = false) = {

    (oldUse.usee.kind, newUsee.kind) match {
      case (Constructor(), Method())
           | (Constructor(), AbstractMethod())=>
        oldUse.user.users().foreach{ AGEdge.uses(_,oldUse.usee).create()}
      case _ => ()
    }

    super.redirectUses(oldUse, newUsee, policy, propagateRedirection, keepOldUse)
  }
}
