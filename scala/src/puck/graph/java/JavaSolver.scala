package puck.graph.java

import puck.graph.constraints.Solver
import puck.graph.AccessGraph._
import puck.graph._
import puck.graph.java.JavaNodeKind.{Field, Method, Class, Interface}

/**
 * Created by lorilan on 28/05/14.
 */
class JavaSolver(val graph : AccessGraph ) extends Solver{

  val violationsKindPriority = List[NodeKind](Field(), Class(), Interface())


}
