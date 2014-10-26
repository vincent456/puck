package puck.util

import java.io.{FileWriter, BufferedWriter, File}

/**
 * Created by lorilan on 08/05/14.
 */

trait Logger[V] {
  def writeln(msg : => Any)(implicit v : V) : Unit
  def write(msg : => Any)(implicit v : V) : Unit
}

trait LogBehavior[V]{
  def mustPrint(v : V) : Boolean
}

trait IntLogBehavior extends LogBehavior[Int]{
  var verboseLevel = 10
  def mustPrint(v : Int) = verboseLevel >= v

}

class NoopLogger[V] extends Logger[V]  {
  def writeln(msg : => Any)(implicit v : V){}
  def write(msg : => Any)(implicit v : V){}
}



trait FileLogger[V] extends Logger[V]{
  this : LogBehavior[V] =>

  val file : File

  private [this] val writer : BufferedWriter =  new BufferedWriter(new FileWriter(file))

  def writeln(msg : => Any)(implicit v : V){
    if(mustPrint(v)) {
      writer.write(msg.toString)
      writer.newLine()
      writer.flush()
    }
  }
  def write(msg : => Any)(implicit v : V){
    if(mustPrint(v)) {
      writer.write(msg.toString)
      writer.flush()
    }
  }
}

class DefaultFileLogger(val file : File) extends FileLogger[Int] with IntLogBehavior


trait SystemLogger[V] extends Logger[V]{
  this : LogBehavior[V] =>

  def writeln(msg : => Any)(implicit v : V){
    if(mustPrint(v)) {
      println(msg)
    }
  }
  def write(msg : => Any)(implicit v : V){
    if(mustPrint(v)) {
      print(msg)
    }
  }
}

object DefaultSystemLogger extends SystemLogger[Int] with IntLogBehavior
