package puck.javaGraph

import java.io.File

import puck.config.ConfigParser
import puck.jastadd.JavaJastAddDG2AST
import puck.util.PuckSystemLogger
import puck.{Project, AcceptanceSpec}

/**
  * Created by lorilan on 25/02/16.
  */
class GraphCompleteness extends AcceptanceSpec {

  feature("fixed number of nodes") {
//    scenario("scenario extracted from freemind problem") {
//      val s0 = new ScenarioFactory("/home/lorilan/test/Test.java")
//      val fnsize = s0.fullName2id.size
//      val nNumber = s0.graph.nodes.size
//      assert(fnsize == nNumber - 1) //root is not in fullName2id
//      Range(0, 100).foreach { _ =>
//        val s = new ScenarioFactory("/home/lorilan/test/Test.java")
//        assert(s.fullName2id.size == fnsize)
//        assert(s.graph.nodes.size == nNumber)
//      }
//    }

    implicit val logger = new PuckSystemLogger(_ => true)// new PuckFileLogger(_ => true, new java.io.File("/tmp/debugLog"))

    scenario("freemind problem") {
      val p =
        new Project(ConfigParser(new File("/home/lorilan/freemind-0.9.0_example/puck.xml")),
              JavaJastAddDG2AST)
      val s0 = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]

      val nBn0size = s0.nodesByName.size
      val n0Number = s0.initialGraph.nodes.size
      assert(nBn0size == n0Number - 1) //root is not in nodesByName
      Range(0, 10).foreach { _ =>
        val s = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]
        val nBnSize = s.nodesByName.size
        val nNumber = s.initialGraph.nodes.size
        assert(nBnSize == nBn0size)
        assert(nNumber == n0Number)
      }
    }
  }
}
