package puck.util

import java.io.{FileWriter, BufferedWriter, File}

/**
 * Created by lorilan on 08/05/14.
 */

trait Logger{
  var verboseLevel = 0

  def writeln(msg : => String, verboseLevelRequiredToDisplay : Int) : Unit
  def write(msg : => String, verboseLevelRequiredToDisplay : Int) : Unit

  def writeln(msg : => String = "" ){writeln(msg, 1)}
  def write(msg : => String) {write(msg, 1)}

}

class NoopLogger extends Logger {
  def writeln(msg : => String, v : Int){}
  def write(msg : => String, v : Int){}

}

class FileLogger(f : File) extends Logger{

  private [this] val writer : BufferedWriter =  new BufferedWriter(new FileWriter(f))

  def writeln(msg : => String, verboseLevelRequiredToDisplay : Int){
    if(verboseLevel >= verboseLevelRequiredToDisplay) {
      writer.write(msg)
      writer.newLine()
      writer.flush()
    }
  }
  def write(msg : => String, verboseLevelRequiredToDisplay : Int){
    if(verboseLevel >= verboseLevelRequiredToDisplay) {
      writer.write(msg)
      writer.flush()
    }
  }
}


class SystemLogger() extends Logger{

  def writeln(msg : => String, verboseLevelRequiredToDisplay : Int){
    if(verboseLevel >= verboseLevelRequiredToDisplay) {
      println(msg)
    }
  }
  def write(msg : => String, verboseLevelRequiredToDisplay : Int){
    if(verboseLevel >= verboseLevelRequiredToDisplay) {
      print(msg)
    }
  }
}