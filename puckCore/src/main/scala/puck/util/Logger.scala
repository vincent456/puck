/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.util

import java.io.{FileWriter, BufferedWriter, File}

trait Logger[V] {
  def writeln(msg : => Any = "")(implicit v : V) : Unit
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
  def writeln(msg : => Any)(implicit v : V): Unit = {}
  def write(msg : => Any)(implicit v : V): Unit = {}
}



trait FileLogger[V] extends Logger[V]{
  this : LogBehavior[V] =>

  val file : File

  private [this] val writer : BufferedWriter =  new BufferedWriter(new FileWriter(file))

  def writeln(msg : => Any)(implicit v : V): Unit = {
    if(mustPrint(v)) {
      writer.write(msg.toString)
      writer.newLine()
      writer.flush()
    }
  }
  def write(msg : => Any)(implicit v : V): Unit = {
    if(mustPrint(v)) {
      writer.write(msg.toString)
      writer.flush()
    }
  }
}

class DefaultFileLogger(val file : File) extends FileLogger[Int] with IntLogBehavior


trait SystemLogger[V] extends Logger[V]{
  this : LogBehavior[V] =>

  def writeln(msg : => Any)(implicit v : V): Unit = {
    if(mustPrint(v)) {
      println(msg)
    }
  }
  def write(msg : => Any)(implicit v : V) : Unit = {
    if(mustPrint(v)) {
      print(msg)
    }
  }
}

object DefaultSystemLogger extends SystemLogger[Int] with IntLogBehavior
