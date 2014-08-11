package puck.util

/**
 * Created by lorilan on 11/08/14.
 */
object Time {
  def time[A, V](logger : Logger[V])(a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    logger.writeln("%d microseconds".format(micros))
    result
  }
}
