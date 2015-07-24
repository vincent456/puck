package puck.javaGraph

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import com.sun.corba.se.pept.transport.ContactInfo
import puck.graph._
import puck.graph.transformations.Recording
import puck.graph.transformations._
import puck.{Settings, AcceptanceSpec}
import nodeKind._

class Toto extends Serializable
class Titi extends Toto


case class WrapToto(n : Toto)
class WrapTiti(override val n : Titi) extends WrapToto(n)


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

    scenario("Tuple"){

      val t = Tuple()
      writeAndClose(tmpFile){
        _.writeObject(t)
      }
      val t2 = readAndClose(tmpFile){
        _.readObject().asInstanceOf[Tuple]
      }

    }


    scenario("case class"){

      try {
        val t = WrapToto(new Titi())
        writeAndClose(tmpFile) {
          _.writeObject(t)
        }
        val t2 = readAndClose(tmpFile) {
          _.readObject().asInstanceOf[WrapToto]
        }
      }
      catch{
        case t : Throwable =>
          t.printStackTrace()
          assert(false)
      }

    }

    scenario("class extending case class"){

      val t = new WrapTiti(new Titi())
      writeAndClose(tmpFile){
        _.writeObject(t)
      }
      val t2 = readAndClose(tmpFile){
        _.readObject().asInstanceOf[WrapTiti]
      }

    }


    scenario("one transformation - add node no type"){

      val t = Transformation(Regular, CNode(ConcreteNode(1, "one", Class, mutable = true)))
      val r1 = t +: Recording()
      writeAndClose(tmpFile){
        Recording.write(_, r1)
      }
      val r2 = readAndClose(tmpFile)(Recording.read)

      assert(r1 == r2)

    }



    scenario("two transformations - add node no type x 2"){

      val t = Transformation(Regular, CNode(ConcreteNode(1, "one", Class, mutable = true)))
      val t2 = Transformation(Regular, CNode(ConcreteNode(2, "two", Class, mutable = true)))

      val r = t +: t2 +: Recording()
      writeAndClose(tmpFile){
        Recording.write(_, r)
      }

      val r2 = readAndClose(tmpFile)(Recording.read)

      assert(r == r2)

    }

    scenario("two transformations - add node with arrow type"){

      val t = Transformation(Regular, CNode(ConcreteNode(1, "one", Method, mutable = true)))
      val t1 = Transformation(Regular, TypeChange(1, None, Some(Arrow(NamedType(0), NamedType(1)))))

      val r1 = t1 +: t +: Recording()
      writeAndClose(tmpFile){
        Recording.write(_, r1)
      }
      val r2 = readAndClose(tmpFile)(Recording.read)

      assert(r1 == r2)

    }

    scenario("four transformation - add node with param to form arrow type"){

      val t = Transformation(Regular, CNode(ConcreteNode(1, "one", Method, mutable = true)))
      val t1 = Transformation(Regular, TypeChange(1, None, Some(NamedType(1))))
      val t2 = Transformation(Regular, CNode(ConcreteNode(2, "p", Param,  mutable = true)))
      val t3 = Transformation(Regular, Edge(ContainsParam(1,2)))


      val r1 =  t3 +: t2 +: t1 +: t +: Recording()
      writeAndClose(tmpFile){
        Recording.write(_, r1)
      }
      val r2 = readAndClose(tmpFile)(Recording.read)

      assert(r1 == r2)

    }

//    scenario("one transformation - add node with method type"){
//
//      val t = Transformation(Regular, CNode(ConcreteNode(1, "one", Method, styp = Some(MethodType(Tuple(List(NamedType(0))), NamedType(1))), mutable = true)))
//      val r1 = t +: Recording()
//      writeAndClose(tmpFile){
//        Recording.write(_, r1)
//      }
//      val r2 = readAndClose(tmpFile)(Recording.read)
//
//      assert(r1 == r2)
//
//    }

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
      gFinal.containsList.toSet should be (gFinalCopy.containsList.toSet)
      gFinal.usesList.toSet should be (gFinalCopy.usesList.toSet)
      gFinal.isaList.toSet should be (gFinalCopy.isaList.toSet)
    }

    scenario("repeat on a rebuilded version of the graph"){
      import bridgeScenario._
      Recording.write(tmpFile, fullName2id, gFinal)

      val bridge2 = BridgeScenario()

      val r2 = Recording.load(tmpFile, bridge2.fullName2id)

      val gFinalCopy = r2.redo(g0)

      bridge2.gFinal.nodes.toSet should be (gFinalCopy.nodes.toSet)
      bridge2.gFinal.containsList.toSet should be (gFinalCopy.containsList.toSet)
      bridge2.gFinal.usesList.toSet should be (gFinalCopy.usesList.toSet)
      bridge2.gFinal.isaList.toSet should be (gFinalCopy.isaList.toSet)
    }
  }
}
