package puck.graph

import puck.FilesHandler
import puck.javaAG.nodeKind.{Primitive, JavaNodeKind}
import scala.collection.mutable.Set
import puck.string2file
/**
 * Created by lorilan on 15/05/14.
 */
class BridgeTestSpec extends UnitSpec{

  val fh = FilesHandler("/home/lorilan/puck_svn/distrib/examples/bridge/hannemann_inspired/candidate")()
  val graph = fh.loadGraph(null)
  println("graph loaded")
  /*fh.accessGraph(fh.parseConstraints())
  println("constraint parsed")*/
  val nodeSet : Set[AGNode[JavaNodeKind]] = Set()

  "The Access Graph of the bridge example" should " iterate over all its node that are not primitive" in {
    nodeSet ++= graph
    // 1 root + 2 packages + 5 classes + 2 methods in screen + 4 * 3 methods in screen subclasses = 22 nodes
    graph.size should be (22)

    graph.nodes.foreach{ node =>
      node.kind match {
      case Primitive() => ()
      case _ => val `nodeSet contains node` = nodeSet contains node
        if(!`nodeSet contains node`){
        println(node.fullName + " not in nodeSet")
        }
        `nodeSet contains node` should be (true)
    }
    }

  }

}
