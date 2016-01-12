package puck.actions

import puck.util._

import scala.swing.Dialog
import scala.swing.Swing.EmptyIcon
import scalaz.Scalaz._

/**
  * Created by lorilan on 12/01/16.
  */
object Choose {

  sealed abstract class DisplayableChoice[+A]{
    def toOption : Option[A]
  }
  case object DisplayableNone extends DisplayableChoice[Nothing]{
    override def toString = "None of the choices above"
    val toOption = None

  }
  case class DisplayableSome[T](value : T) extends DisplayableChoice[T]{
    override def toString = value.toString
    def toOption = Some(value)
  }


  def apply[T](title : String,
                msg : Any,
                choices : Seq[T],
                k : Logged[Option[T]] => Unit,
                appendNone : Boolean = false) : Unit = {

    choices match {
      case Seq() => k(none[T].set(""))
      case Seq(x) if !appendNone => k(some(x).set(""))
      case _ =>
        val sChoices = choices.map(DisplayableSome(_))
        Dialog.showInput(null, msg, title,
          Dialog.Message.Plain,
          icon = EmptyIcon,
          if(appendNone) DisplayableNone +: sChoices else sChoices, sChoices.head) match {
          case None => () //Cancel
          case Some(x) => k(x.toOption.set(""))
        }
    }

  }
}
