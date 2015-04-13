package puck.javaGraph

import puck.{Settings, AcceptanceSpec}
import puck.graph.ConcreteNode
import puck.graph.transformations.{Recording, Add, TTCNode, Transformation}
import nodeKind._

class RecordingSerializationSpec extends AcceptanceSpec {

  val tmpFile = Settings.tmpDir + "tmpFile"
  feature("Serialization"){


    scenario("one transformation"){

      val t = Transformation(Add, TTCNode(ConcreteNode(1, "one", Class, styp = None, mutable = true)))

      val map = Map("one" -> 1)

      Recording.write(tmpFile, map, t +: Recording())

      val r = Recording.load(tmpFile, map)

      assert(r.head == t)

    }

    scenario("two transformation"){

      val t = Transformation(Add, TTCNode(ConcreteNode(1, "one", Class, styp = None, mutable = true)))
      val t2 = Transformation(Add, TTCNode(ConcreteNode(2, "two", Class, styp = None, mutable = true)))

      val r = t +: t2 +: Recording()

      val map = Map("one" -> 1,
                    "two" -> 2)

      Recording.write(tmpFile, map, r)

      val r2 = Recording.load(tmpFile, map)

      assert(r == r2)

    }
  }
}
