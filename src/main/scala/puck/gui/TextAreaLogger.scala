package puck.gui

import puck.util.{PuckLogger, PuckLog}

import scala.swing.TextArea

class TextAreaLogger
( val console : TextArea,
  val askPrint : PuckLog.Verbosity => Boolean
  ) extends PuckLogger {

  def writeln(msg : => Any)(implicit v : PuckLog.Verbosity) : Unit = {
    if(mustPrint(v)) {
      console.append(preMsg(v) + msg)
      console.append(System.lineSeparator())
    }
  }
  def write(msg : => Any)(implicit v : PuckLog.Verbosity) : Unit = {
    if(mustPrint(v)) {
      console.append(preMsg(v) + msg)
    }
  }
}
