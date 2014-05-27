package puck.graph.constraints

import scala.util.parsing.combinator.RegexParsers
import puck.graph.{AGNode, AccessGraph}
import scala.collection.mutable
import scala.util.parsing.input.{Reader, StreamReader}


/**
 * Created by lorilan on 12/05/14.
 */
class ConstraintsParser(val accessGraph : AccessGraph) extends RegexParsers {

  protected override val whiteSpace = """(\s|%.*)+""".r  //to skip comments


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
    """[^\[\]\s',().]+""".r
  | "'" ~> """[^\s']+""".r <~ "'"
  )
  def list : Parser[List[String]] = "[" ~> repsep(ident, ",") <~ "]"     //rep1sep ??

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
        case None => defs += (ident -> (list map findNode))
      }
    }

  def declare_set_union : Parser[Unit] =
    "declareSetUnion(" ~> ident ~ "," ~ list <~ ")." ^^ {
      case ident ~ _ ~ list => defs get ident match {
        case Some(_) => throw new scala.Error("Set " + ident + " already defined")
        case None => defs += (ident -> (list map {defs}).flatten )
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

  def hideScopeSet1 : Parser[List[HideScope]] =
    "hideScopeSet(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        toDef(s) map {
          HideScope(_, List(), List(accessGraph.root), List())
        }
    }

  def hideScopeSetFrom : Parser[List[HideScope]] =
    "hideScopeSetFrom(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case s ~ _ ~ interlopers =>
        val is = toDef(interlopers)
        toDef(s) map {
          HideScope(_, List(), is, List())
        }
    }


  def friend : Parser[List[AreFriendsOf]] =
    "isFriendOf(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case friends ~ _ ~ befriended =>
         val fs = toDef(friends)
        toDef(befriended) map {
          AreFriendsOf(fs, _)
        }
  }

  def hideElement : Parser[HideElement] =
    "hide(" ~> ident ~ "," ~ listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case elt ~ _ ~ interlopers ~ _ ~ friends =>
      HideElement(findNode(elt), toDef(interlopers), toDef(friends))
    }

  def hideElementSet : Parser[List[HideElement]] =
    "hideSet(" ~> listOrIdent ~ "," ~ listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case set ~ _ ~ interlopers ~ _ ~ friends =>
        val is = toDef(interlopers)
        val fs = toDef(friends)
        toDef(set) map {
          HideElement(_, is, fs)
        }

  }

  def constraints : Parser[Option[List[Constraint]]] = {
    def some(x : List[Constraint]) = Some(x)
    def somelist(x : Constraint) = Some(List(x))
    ( java_import ^^ ( x => None)
      | declare_set ^^ (x => None)
      | declare_set_union ^^ (x => None )
      | hideElement ^^ somelist
      | hideElementSet ^^ some
      | hideScope ^^ somelist
      | hideScopeSet1 ^^ some
      | hideScopeSetFrom ^^ some
    )}

  /*def apply(input : java.io.Reader) = parseAll(constraints, input) match{
    case Success(result, _ ) => result
    case failure : NoSuccess => throw new scala.Error(failure.msg)
  }*/

  def apply(input : java.io.Reader) = {
    def aux(input : Reader[Char], acc: List[Constraint]) : List[Constraint] =
      parse(constraints, input) match {
        case Success(r, i) => r match {
          case None => aux(i, acc)
          case Some(res) => aux(i, res ::: acc)
        }
        case Error(msg, next) =>
          throw new scala.Error("!!! Error !!! at position " + next.pos + " " + msg)
        case Failure(msg, next) =>
          if(next.atEnd) acc
          else throw new scala.Error("Failure at position " + next.pos + " " + msg )
    }
    aux(StreamReader(input), List())
  }
}
