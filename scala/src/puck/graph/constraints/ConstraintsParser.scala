package puck.graph.constraints

import scala.util.parsing.combinator.RegexParsers
import puck.graph.{AGNode, AccessGraph}
import scala.collection.mutable
import scala.util.parsing.input.{Reader, StreamReader}

/**
 * Created by lorilan on 12/05/14.
 */
class ConstraintsParser(val accessGraph : AccessGraph) extends RegexParsers{

  val defs : mutable.Map[String, List[AGNode]] = mutable.Map[String, List[AGNode]]()
  val imports : mutable.Buffer[String] = mutable.Buffer("")

  def findNode(k : String) : AGNode ={
    case class Found(n : AGNode) extends scala.Error
    try {
      imports foreach { (imp : String) =>
        accessGraph getNode (imp + k) match {
          case None => ()
          case Some(n) => throw new Found(n)
        }
      }
      throw new NoSuchElementException(k + " not found")
    }
    catch{
      case Found(n) => n
    }
  }

  def toDef (request : Either[String, List[String]]) : List[AGNode]= {
    request match {
      case Left(key) => defs get key match {
        case Some(l) => l
        case None => List(findNode(key))
      }
      case Right(l) => l map findNode
    }
  }

  def ident : Parser[String] = (
    """[^\s',().]+""".r
  | "'" ~> """[^\s']+""".r <~ "'"
  )
  def list : Parser[List[String]] = "[" ~> rep1sep(ident, ",") <~ "]"

  def listOrIdent : Parser[Either[String, List[String]]] = (
    list  ^^ (Right(_))
      | ident  ^^ (Left(_))
    )

  def java_import : Parser[Unit] =
    "java_import(" ~> list <~ ")." ^^ (_.foreach{str => imports += (str +".")})


  def declare_set : Parser[Unit] =
    "declareSet(" ~> ident ~ "," ~ list <~ ")." ^^ {
      case ident ~ _ ~ list => defs get ident match {
        case Some(_) => throw new scala.Error("Set " + ident + " already defined")
        case None => defs += ((ident, list map findNode))
      }
    }

  def hideScope : Parser[HideScope] =
    "hideScope(" ~> ident ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent <~ ")." ^^ {
      case  i ~ _ ~ facades ~ _ ~ interlopers ~ _ ~ friends =>
        HideScope(findNode(i), toDef(facades), toDef(interlopers), toDef(friends))
    }

  def hideScopeSetFrom : Parser[List[HideScope]] =
    "hideScopeSetFrom(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case s ~ _ ~ interlopers =>
        val is = toDef(interlopers)
        toDef(s) map {
          HideScope(_, List(), is, List())
        }
    }

  def constraints : Parser[Option[List[HideScope]]] = (
    java_import ^^ ( x => None)
      | declare_set ^^ (x => None)
      | hideScope ^^ (x => Some(List(x)))
      | hideScopeSetFrom ^^ (x => Some(x))
    )

  /*def apply(input : java.io.Reader) = parseAll(constraints, input) match{
    case Success(result, _ ) => result
    case failure : NoSuccess => throw new scala.Error(failure.msg)
  }*/

  def apply(input : java.io.Reader) = {
    def aux(input : Reader[Char], acc: List[HideScope]) : List[HideScope] =
      if(input.atEnd) acc
      else parse(constraints, input) match {
        case Success(r, i) => r match {
          case None => aux(i, acc)
          case Some(res) => aux(i, res ::: acc)
        }
        case NoSuccess(msg, i) => throw new scala.Error("at position " + i.pos + " " + msg)
      }
    aux(StreamReader(input), List())
  }
}
