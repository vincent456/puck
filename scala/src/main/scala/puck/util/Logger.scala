package puck.util

import java.io.{FileWriter, BufferedWriter, File}

/**
 * Created by lorilan on 08/05/14.
 */

trait Logger[V]{

  protected def mustPrint(v : V) : Boolean

  def writeln(msg : => String, v : V) : Unit
  def write(msg : => String, v : V) : Unit

  def writeln(msg : => String = "" ): Unit
  def write(msg : => String) : Unit

}

trait IntLogger extends Logger[Int]{
  var verboseLevel = 10
  def mustPrint(v : Int) = verboseLevel >= v

  def writeln(msg : => String = "" ){writeln(msg, 1)}
  def write(msg : => String){write(msg, 1)}
}


class NoopLogger[V] extends Logger[V] {
  def mustPrint(v : V) = false
  def writeln(msg : => String, v : V){}
  def write(msg : => String, v : V){}
  def writeln(msg : => String = "" ){}
  def write(msg : => String) {}
}

trait FileLogger[V] extends Logger[V]{

  val file : File

  private [this] val writer : BufferedWriter =  new BufferedWriter(new FileWriter(file))

  def writeln(msg : => String, v : V){
    if(mustPrint(v)) {
      writer.write(msg)
      writer.newLine()
      writer.flush()
    }
  }
  def write(msg : => String, v : V){
    if(mustPrint(v)) {
      writer.write(msg)
      writer.flush()
    }
  }
}

class DefaultFileLogger(val file : File) extends FileLogger[Int] with IntLogger


trait SystemLogger[V] extends Logger[V]{

  def writeln(msg : => String, v : V){
    if(mustPrint(v)) {
      println(msg)
    }
  }
  def write(msg : => String, v : V){
    if(mustPrint(v)) {
      print(msg)
    }
  }
}

object DefaultSystemLogger extends SystemLogger[Int] with IntLogger
