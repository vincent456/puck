/**
  * Created by cedric on 07/07/2016.
  */

import oscar.cp._

object Essai3 extends CPModel with App {

  var v1 = CPIntVar(1 to 5)
  var v2 = CPIntVar(1 to 5)

  var essai:Set[CPIntVar] = Set(v1)
  var i = 0

  add(sum(essai) < 10)

  search {
    binaryFirstFail(Seq(v1)++Seq(v2))
  } onSolution {
    println(essai.mkString(" "))
      essai+=v2
  }
  start()
}