/**
  * Created by cedric on 07/07/2016.
  */
import org.jfree.data.Values
import oscar.cp._

object Essai extends CPModel with App {

  var v1 = CPIntVar(1 to 10)
  var v2 = CPIntVar(1 to 10)

  var values = Seq(v1)
  add(sum(values) <= 10)
  def recherche (values : Seq[CPIntVar], m : String) = {
    search {
      binaryFirstFail(values)
    } onSolution {
      println(m + "  somme = " +sum(values) + " values = "+ values.mkString(" "))
    }
  }
  add(sum(values) <= 10)
  recherche(values, "S1")
  values++=Seq(v2)
  add(sum(values) <= 10)
  recherche(values, "S2")

  start()

}
