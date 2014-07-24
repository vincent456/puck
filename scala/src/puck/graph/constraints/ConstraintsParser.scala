package puck.graph.constraints

import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable
import scala.util.parsing.input.{Reader, StreamReader}
import puck.graph.{NodeKind, AGNode, AccessGraph}


/**
 * Created by lorilan on 12/05/14.
 */
class ConstraintsParser[Kind <: NodeKind[Kind]](val accessGraph : AccessGraph[Kind]) extends RegexParsers {

  protected override val whiteSpace = """(\s|%.*)+""".r  //to skip comments


  val defs : mutable.Map[String, NamedNodeSet[Kind]] = accessGraph.nodeSets

  val imports : mutable.Buffer[String] = mutable.Buffer("")

  def findNode(k : String) : AGNode[Kind] ={
    case class Found(n : AGNode[Kind]) extends Throwable
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

  def toDef (request : Either[String, List[String]]) : NodeSet[Kind] = {
    request match {
      case Left(key) => defs get key match {
        case Some(l) => l
        case None => LiteralNodeSet(findNode(key))
      }
      case Right(l) => LiteralNodeSet(l map findNode)
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
        case None => defs += (ident -> new NamedNodeSet(ident,
          LiteralNodeSet(list map findNode)))
      }
    }

  def declare_set_union : Parser[Unit] = {


    def normal(list : List[NodeSet[Kind]]) = {

      val lit = LiteralNodeSet[Kind]()
      val buf = mutable.Buffer[NodeSet[Kind]]()

      list.foreach{
        case l : LiteralNodeSet[Kind] => l.foreach(lit.+=)
        case s => buf += s
      }
      new NodeSetUnion(buf, lit)
    }

    "declareSetUnion(" ~> ident ~ "," ~ list <~ ")." ^^ {
      case ident ~ _ ~ list => defs get ident match {
        case Some(_) => throw new scala.Error("Set " + ident + " already defined")
        case None => defs +=(ident -> new NamedNodeSetUnion(ident, normal(list map defs)))
      }
    }
  }

  def addScopeConstraint(owners : NodeSet[Kind],
                         facades : NodeSet[Kind],
                         interlopers : NodeSet[Kind],
                         friends : NodeSet[Kind]) = {
    val ct = new ScopeConstraint(owners, facades, interlopers, friends)
    owners.foreach(_.scopeConstraints.+=(ct))
    accessGraph.constraints += ct
  }

  def hideScope : Parser[Unit] =
    "hideScope(" ~> ident ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent <~ ")." ^^ {
      case  i ~ _ ~ facades ~ _ ~ interlopers ~ _ ~ friends =>
        addScopeConstraint(LiteralNodeSet(findNode(i)),
          toDef(facades), toDef(interlopers), toDef(friends))
    }



  def hideScopeSet1 : Parser[Unit] =
    "hideScopeSet(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        addScopeConstraint(toDef(s), LiteralNodeSet(),
          LiteralNodeSet(accessGraph.root), LiteralNodeSet())
    }

  def hideScopeSet4 : Parser[Unit] =
    "hideScopeSet(" ~> listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent <~ ")." ^^ {
      case s ~ _ ~ facades ~ _ ~ interlopers ~ _ ~ friends =>
        addScopeConstraint(toDef(s), toDef(facades),
          toDef(interlopers), toDef(friends))
    }


  def hideScopeSetFrom : Parser[Unit] =
    "hideScopeSetFrom(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case s ~ _ ~ interlopers =>
        addScopeConstraint(toDef(s), LiteralNodeSet(),
          toDef(interlopers), LiteralNodeSet())
    }

  def hideScopeFromEachOther : Parser[Unit] = {
    "hideFromEachOther(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        val owners = toDef(s)
        addScopeConstraint(owners, LiteralNodeSet(),
          owners, LiteralNodeSet())
    }
  }

  def friend : Parser[Unit] =
    "isFriendOf(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case friends ~ _ ~ befriended =>
        val owners = toDef(befriended)
        val ct = new FriendConstraint(toDef(friends), owners)
        owners.foreach(_.friendConstraints += ct)
        accessGraph.constraints += ct
    }

  def addElementConstraint(owners : NodeSet[Kind],
                           interlopers : NodeSet[Kind],
                           friends : NodeSet[Kind]) = {
    val ct = new ElementConstraint(owners, interlopers, friends)
    owners.foreach(_.elementConstraints.+=(ct))
    accessGraph.constraints += ct
  }

  def hideElement3 : Parser[Unit] =
    "hide(" ~> ident ~ "," ~ listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case elt ~ _ ~ interlopers ~ _ ~ friends =>
        addElementConstraint(LiteralNodeSet(findNode(elt)),
          toDef(interlopers), toDef(friends))
    }

  def hideElementSet1 : Parser[Unit] =
    "hideSet(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        addElementConstraint(toDef(s),
          LiteralNodeSet(accessGraph.root), LiteralNodeSet())
    }

  def hideElementSet3 : Parser[Unit] =
    "hideSet(" ~> listOrIdent ~ "," ~ listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case set ~ _ ~ interlopers ~ _ ~ friends =>
        addElementConstraint(toDef(set),
          toDef(interlopers), toDef(friends))
  }

  def hideElementFrom : Parser[Unit] =
    "hideFrom(" ~> ident ~ "," ~ listOrIdent <~ ")." ^^ {
    case elt ~ _ ~ interlopers =>
      addElementConstraint(LiteralNodeSet(findNode(elt)),
        toDef(interlopers), LiteralNodeSet())
  }

  def hideElementSetFrom : Parser[Unit] =
    "hideSetFrom(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case s ~ _ ~ interlopers =>
        addElementConstraint(toDef(s),
          toDef(interlopers), LiteralNodeSet())
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
      | hideElement3
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
