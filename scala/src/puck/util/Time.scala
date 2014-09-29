package puck.util

/**
 * Created by lorilan on 11/08/14.
 */
object Time {
  def time[A, V](logger : Logger[V])(a: => A) = {
    val now = System.nanoTime
    val result = a
    val secTotal : Double = (System.nanoTime - now).toDouble / 1000000000
    val h : Int = (secTotal / 3600).toInt
    val m : Int = ((secTotal - (h*3600)) / 60).toInt

    val s0 : Double = secTotal - (h*3600) - (m * 60)
    val s = s0.toInt
    val micros = ((s0 - s.toDouble) * 1000000).toInt

    logger.writeln(" %dh %dm %ds %d microseconds".format(h, m, s, micros))
    result
  }
}
