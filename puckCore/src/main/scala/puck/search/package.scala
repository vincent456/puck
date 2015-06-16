package puck

import scalaz.\/

/**
 * Created by lorilan on 2/23/15.
 */
package object search {
  type Try[T] = PuckError \/ T
}
