package puck.graph

import scala.collection.mutable

/**
 * Created by lorilan on 05/05/14.
 */
class AccessGraph {
    private[graph] val nodes : mutable.Map[Int, AGNode] = new mutable.HashMap[Int, AGNode]()
}
