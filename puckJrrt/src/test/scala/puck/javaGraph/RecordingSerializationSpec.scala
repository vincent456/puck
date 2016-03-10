/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.javaGraph

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import puck.graph._
import puck.graph.transformations._
import puck.graph.transformations.Recording
import Recording.RecordingOps
import puck.{Settings, AcceptanceSpec}
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
