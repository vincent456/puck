package puck.javaGraph

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import puck.{Settings, AcceptanceSpec}
import puck.graph._
import puck.graph.transformations.{Recording, Regular, CNode, Transformation}
import nodeKind._

class RecordingSerializationSpec extends AcceptanceSpec {

  val tmpFile = Settings.tmpDir + "tmpFile"

  def readAndClose[T](fileName : String)(f : ObjectInputStream => T) : T = {
    val ois = new ObjectInputStream(new FileInputStream(fileName))
    val t = f(ois)
    ois.close()
    t
  }

  def writeAndClose(fileName : String)(f : ObjectOutputStream => Unit) = {
    val ois = new ObjectOutputStream(new FileOutputStream(fileName))
    f(ois)
    ois.close()
  }



  feature("Serialization"){

    scenario("one transformation"){

      val t = Transformation(Regular, CNode(ConcreteNode(1, "one", Class, styp = None, mutable = true)))
      val r1 = t +: Recording()
      writeAndClose(tmpFile){
        Recording.write(_, r1)
      }
      val r2 = readAndClose(tmpFile)(Recording.read)

      assert(r1 == r2)

    }

    scenario("two transformations"){

      val t = Transformation(Regular, CNode(ConcreteNode(1, "one", Class, styp = None, mutable = true)))
      val t2 = Transformation(Regular, CNode(ConcreteNode(2, "two", Class, styp = None, mutable = true)))

      val r = t +: t2 +: Recording()
      writeAndClose(tmpFile){
        Recording.write(_, r)
      }

      val r2 = readAndClose(tmpFile)(Recording.read)

      assert(r == r2)

    }

    val bridgeScenario = BridgeScenario()

    scenario("numerous transformations"){
      import bridgeScenario._

      writeAndClose(tmpFile){
        Recording.write(_, gFinal.recording)
      }
      val r2 = readAndClose(tmpFile)(Recording.read)

      assert(gFinal.recording == r2)

    }

    scenario("repeat on same graph (no rebuilding)"){
      import bridgeScenario._

      writeAndClose(tmpFile){
        Recording.write(_, gFinal.recording)
      }
      val r2 = readAndClose(tmpFile)(Recording.read)

      val gFinalCopy = r2.redo(g0)

      gFinal.nodes.toSet should be (gFinalCopy.nodes.toSet)
      gFinal.containsSeq.toSet should be (gFinalCopy.containsList.toSet)
      gFinal.usesSeq.toSet should be (gFinalCopy.usesList.toSet)
      gFinal.isaSeq.toSet should be (gFinalCopy.isaList.toSet)
    }

    scenario("repeat on a rebuilded version of the graph"){
      import bridgeScenario._
      Recording.write(tmpFile, fullName2id, gFinal)

      val bridge2 = BridgeScenario()

      val r2 = Recording.load(tmpFile, bridge2.fullName2id)

      val gFinalCopy = r2.redo(g0)

      bridge2.gFinal.nodes.toSet should be (gFinalCopy.nodes.toSet)
      bridge2.gFinal.containsSeq.toSet should be (gFinalCopy.containsList.toSet)
      bridge2.gFinal.usesSeq.toSet should be (gFinalCopy.usesList.toSet)
      bridge2.gFinal.isaSeq.toSet should be (gFinalCopy.isaList.toSet)
    }
  }
}
