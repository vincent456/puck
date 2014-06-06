package puck.graph.constraints

import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable
import scala.util.parsing.input.{Reader, StreamReader}
import scala.Some
import puck.graph.{AGNode, AccessGraph}


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
        case None => defs += (ident -> (list map defs).flatten )
      }
    }

  def hideScope : Parser[Unit] =
    "hideScope(" ~> ident ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent <~ ")." ^^ {
      case  i ~ _ ~ facades ~ _ ~ interlopers ~ _ ~ friends =>
        findNode(i) scopeConstraints_+= (toDef(facades), toDef(interlopers), toDef(friends))
    }



  def hideScopeSet1 : Parser[Unit] =
    "hideScopeSet(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        toDef(s) map {_.scopeConstraints_+=(List(), List(accessGraph.root), List()) }
    }

  def hideScopeSet4 : Parser[Unit] =
    "hideScopeSet(" ~> listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent <~ ")." ^^ {
      case s ~ _ ~ facades ~ _ ~ interlopers ~ _ ~ friends =>
        val fcs = toDef(facades)
        val is = toDef(interlopers)
        val frs = toDef(friends)
        toDef(s) map { _.scopeConstraints_+=(fcs, is, frs)}
    }


  def hideScopeSetFrom : Parser[Unit] =
    "hideScopeSetFrom(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case s ~ _ ~ interlopers =>
        val is = toDef(interlopers)
        toDef(s) map {_.scopeConstraints_+=(List(), is, List())}
    }

  def hideScopeFromEachOther : Parser[Unit] = {
    "hideFromEachOther(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        val d = toDef(s)
        d.foreach{ node =>
          val others = d.filter(_ != node)
          node.scopeConstraints_+=(List(), others, List())
        }
    }
  }

  def friend : Parser[Unit] =
    "isFriendOf(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case friends ~ _ ~ befriended =>
        val fs = toDef(friends)
        toDef(befriended) map { _ friends_++= fs }
    }

  def hideElement : Parser[Unit] =
    "hide(" ~> ident ~ "," ~ listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case elt ~ _ ~ interlopers ~ _ ~ friends =>
        findNode(elt).elementConstraints_+=(toDef(interlopers), toDef(friends))
    }

  def hideElementSet1 : Parser[Unit] =
    "hideSet(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        toDef(s) map {_.elementConstraints_+=(List(accessGraph.root), List())}
    }

  def hideElementSet3 : Parser[Unit] =
    "hideSet(" ~> listOrIdent ~ "," ~ listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case set ~ _ ~ interlopers ~ _ ~ friends =>
        val is = toDef(interlopers)
        val fs = toDef(friends)
        toDef(set) map {_.elementConstraints_+=(is, fs)}
    }

  def hideElementFrom : Parser[Unit] =
    "hideFrom(" ~> ident ~ "," ~ listOrIdent <~ ")." ^^ {
    case elt ~ _ ~ is =>
        findNode(elt).elementConstraints_+=(toDef(is), List())
  }
  def hideElementSetFrom : Parser[Unit] =
    "hideSetFrom(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case set ~ _ ~ interlopers =>
        val is = toDef(interlopers)
        toDef(set).foreach{ _.elementConstraints_+=(is, List())}
    }


  /*def constraints : Parser[Option[List[Constraint]]] = {
    def some(x : List[Constraint]) = Some(x)
    def somelist(x : Constraint) = Some(List(x))
    ( java_import ^^ ( x => None)
      | declare_set ^^ (x => None)
      | declare_set_union ^^ (x => None )
      | hideElement ^^ somelist
      | hideElementSet ^^ some
      | hideScope ^^ somelist
      | hideScopeSet ^^ some
      | hideScopeSetFrom ^^ some
      | friend ^^ some
      )}*/

  def constraints : Parser[Unit] = {
    ( java_import
      | declare_set
      | declare_set_union
      | hideElement
      | hideElementFrom
      | hideElementSet1
      | hideElementSet3
      | hideElementSetFrom
      | hideScope
      | hideScopeSet1
      | hideScopeSet4
      | hideScopeSetFrom
      | hideScopeFromEachOther
      | friend
      )}

  /*def apply(input : java.io.Reader) = parseAll(constraints, input) match{
    case Success(result, _ ) => result
    case failure : NoSuccess => throw new scala.Error(failure.msg)
  }*/

  def apply(input : java.io.Reader) = {
    def aux(input : Reader[Char]) {
      parse(constraints, input) match {
        case Success(_, i) => aux(i)
        case Error(msg, next) =>
          throw new scala.Error("!!! Error !!! at position " + next.pos + " " + msg)
        case Failure(msg, next) =>
          if (next.atEnd) ()
          else throw new scala.Error("Failure at position " + next.pos + " " + msg)
      }
    }
    aux(StreamReader(input))
  }
}
