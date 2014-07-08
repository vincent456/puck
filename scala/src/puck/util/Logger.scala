package puck.util

import java.io.{FileWriter, BufferedWriter, File}

/**
 * Created by lorilan on 08/05/14.
 */

trait Logger{
  def writeln(msg : String) : Unit
  def write(msg : String) : Unit
  def log[T](op : => T) : T
}

class NoopLogger extends Logger {
  def writeln(msg : String){}
  def write(msg : String){}

  def log[T](op : => T) : T = op
}

class FileLogger(val f : File) extends Logger{
  private [this] var writter : BufferedWriter = _

  def writeln(msg : String){
    writter.write(msg)
    writter.newLine()
  }
  def write(msg : String){
    writter.write(msg)
  }

  def log[T](op : => T) : T = {
    writter = new BufferedWriter(new FileWriter(f))

    val res = op

    writter.close()

    res
  }
}


