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

package puck.actions

import puck.util._

import scala.swing.{Component, Dialog}
import scala.swing.Swing.EmptyIcon
import scalaz.Scalaz._

/**
  * Created by Loïc Girault on 12/01/16.
  */
object Choose {

  sealed abstract class DisplayableChoice[+A]{
    def toOption : Option[A]
  }
  case object DisplayableNone extends DisplayableChoice[Nothing]{
    override def toString = "None of the choices below"
    val toOption = None

  }
  case class DisplayableSome[T](value : T) extends DisplayableChoice[T]{
    override def toString = value.toString
    def toOption = Some(value)
  }

  def apply[T](title : String,
               msg : Any,
               choices : Seq[T]) : Option[T] =
  Dialog.showInput(null, msg, title,
    Dialog.Message.Plain,
    icon = EmptyIcon, choices, choices.head)

  def apply[T]( title : String,
                msg : Any,
                choices : Seq[T],
                k : Logged[Option[T]] => Unit,
                appendNone : Boolean = false) : Unit = {

    choices match {
      case Seq() => k(none[T].set(""))
      case Seq(x) if !appendNone => k(some(x).set(""))
      case _ =>
        val sChoices = choices.map(DisplayableSome(_))
          apply(title, msg,
          if(appendNone) DisplayableNone +: sChoices else sChoices) match {
          case None => () //Cancel
          case Some(x) => k(x.toOption.set(""))
        }
    }

  }
}
