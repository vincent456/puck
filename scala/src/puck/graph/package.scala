package puck

import scala.language.implicitConversions
/**
 * Created by lorilan on 15/05/14.
 */
package object graph {
     implicit def agToIterator(ag : AccessGraph) : Iterator[AGNode] = ag.iterator
}
