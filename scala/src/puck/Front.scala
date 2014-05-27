package puck


import scala.swing._
import javax.swing.UIManager
import puck.gui.PuckMainPanel
import java.io.OutputStream
import puck.graph.AccessGraph

/**
 * Created by lorilan on 08/05/14.
 */

object Front extends SwingApplication{

  override def startup(args: Array[String]){
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    if (top.size == new Dimension(0,0)) top.pack()
    top.visible = true
  }

  def top = new MainFrame {
    title = "Puck"

    size = new Dimension(300, 200)

    contents  = new PuckMainPanel(FilesHandler())

  }
}

/*
object Front{

  def main(args : Array[String]){
    val fh = FilesHandler("/home/lorilan/puck_svn/distrib/examples/bridge/hannemann/candidate")()
    val graph = fh.loadGraph(null)
    println("graph loaded")
    //fh.accessGraph.list()
    //println(fh.parseConstraints() == fh.parseConstraints())
    graph(fh.parseConstraints())

    /*def test(i :Int) : Unit = {

      def aux (i : Int, res : List[Set[AccessGraph.Violation]]) : Unit = {
        if( i >0) {
          graph.discardConstraints ()

          graph (fh.parseConstraints () )
          val v = graph.violations

          res.foreach{ (v0) =>
            println( v0 == v)
          }
          aux(i - 1, v :: res)
        }
      }
      aux(i, List())
    }

    test(10)*/

    /*val fh2 = FilesHandler("/home/lorilan/puck_svn/distrib/examples/bridge/hannemann_inspired/candidate")()
    fh2.loadGraph(null)
    println("graph loaded - 2")
    fh2.accessGraph(fh2.parseConstraints())

    val v1 = fh.accessGraph.violations
    val v2 = fh2.accessGraph.violations

    println(v1 == v2)*/

    /*println(v1.mkString("set1:", "\n", "***"))
    println(v2.mkString("set2:", "\n", "***"))
*/
    //fh.accessGraph.printConstraints()

    //println("violations: ")
    //println(fh.accessGraph.violations.mkString("\n"))

    println("make dot ...")
    fh.makeDot()
    println("dot to png...")
    fh.dot2png()


  }
}
*/
