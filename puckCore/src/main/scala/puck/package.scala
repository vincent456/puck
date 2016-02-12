/**
 * Created by lorilan on 04/11/15.
 */
package object puck {

  @inline
  def ignore[A](a : => A) : Unit ={
    val _ = a
  }

  def error(str : String) : Nothing = throw new PuckError(str)
  def error() : Nothing = error("should not happen !")
}
