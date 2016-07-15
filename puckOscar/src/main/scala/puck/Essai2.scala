/**
  * Created by cedric on 07/07/2016.
  */
import oscar.cp._

import scala.collection.mutable.ArrayBuffer

object Essai2 extends CPModel with App {

  var variable = CPIntVar(1 to 10)

  var essai = ArrayBuffer(variable)

  add(sum(essai) < 10)

  search {
    binaryFirstFail(essai)
  } onSolution {
    println("somme = "+sum(essai) + " essai = "+ essai.mkString(" "))
    essai+=variable
  }
  start()
}