package puck

import scalaz._

package object util {
  type Logged[A] = Writer[String, A]
}
