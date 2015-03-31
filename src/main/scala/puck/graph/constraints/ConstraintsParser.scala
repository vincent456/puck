package puck.graph
package constraints

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Reader, StreamReader}


sealed trait ParsedId {
  val node : String
  def range : NodeId => Range
}
case class SetIdOrScope(node:String) extends ParsedId {
  def range = id => Scope(id)
}
case class PElement(node:String)extends ParsedId {
  def range = id => Element(id)
}

/**
 * Created by lorilan on 12/05/14.
 */

object ConstraintsParser{
  def apply(builder : GraphBuilder) =
    new ConstraintsParser(builder)
}
class ConstraintsParser private
( val builder : GraphBuilder) extends RegexParsers {

  builder.discardConstraints()
  protected override val whiteSpace = """(\s|%.*|#.*)+""".r  //to skip comments


  var defs : Map[String, NamedRangeSet] = Map()


  var imports : Seq[String] = Seq("")

  def findNode(k : ParsedId) : Seq[Range] ={

    def aux(imports : Seq[String], acc : Seq[Range]) : Seq[Range] =
      imports match {
        case Seq() if acc.isEmpty => throw new NoSuchElementException("named element " + k + " not found")
        case Seq() => acc
        case _ =>
          builder.nodesByName get (imports.head + k.node) match {
            case None => aux(imports.tail, acc)
            case Some(n) => aux(imports.tail, k.range(n) +: acc)
          }
      }

    aux(imports, Seq())
  }

  def toDef (request : Either[ParsedId, Seq[ParsedId]]) : RangeSet = {
    request match {
      case Left(key) => defs get key.node match {
        case Some(l) => l
        case None => LiteralRangeSet(findNode(key))
      }
      case Right(l) => LiteralRangeSet(l flatMap findNode)
    }
  }

  def ident : Parser[String] = (
    """[^\[\]\s',().]+""".r
      | "'" ~> """[^\s']+""".r <~ "'"
    )

  def range : Parser[ParsedId] =(
     "r:" ~> ident ^^ (PElement(_))
    | ident ^^ (SetIdOrScope(_))
  )
  def rangeList : Parser[List[ParsedId]] = (
    "[" ~> repsep(range, ",") <~ "]"     //rep1sep ??
      | "r:[" ~> repsep(range, ",") <~ "]" ^^ { _.map(e => PElement(e.node))}
    )
  def rangeListOrRange : Parser[Either[ParsedId, List[ParsedId]]] = (
      rangeList  ^^ (Right(_))
      | range  ^^ (Left(_))
    )

  def list : Parser[List[String]] = "[" ~> repsep(ident, ",") <~ "]"     //rep1sep ??

  /*def listOrIdent : Parser[Either[String, List[String]]] = (
      list  ^^ (Right(_))
      | ident  ^^ (Left(_))
    )
*/
  def java_import : Parser[Unit] =
    "java_import(" ~> list <~ ")." ^^ { l : List[String] =>
      imports = l.foldLeft(imports) {case (acc, str) => (str +".") +: acc}
    }

  def declare_set : Parser[Unit] =
    "declareSet(" ~> ident ~ "," ~ rangeList <~ ")." ^^ {
      case ident ~ _ ~ list => defs get ident match {
        case Some(_) => throw new scala.Error("Set " + ident + " already defined")
        case None => defs += (ident -> new NamedRangeSet(ident,
          LiteralRangeSet(list flatMap findNode)))
      }
    }

  def declare_set_union : Parser[Unit] = {

    def normal(list : Seq[RangeSet]) = {

      val (namedSets, lit) = list.foldLeft(( Seq[RangeSet](),LiteralRangeSet())){
        case ((nsAcc, litAcc), l : LiteralRangeSet) => (nsAcc, litAcc ++ l)
        case ((nsAcc, litAcc), ns) => (ns +: nsAcc, litAcc)
      }

      new RangeSetUnion(namedSets, lit)
    }

    "declareSetUnion(" ~> ident ~ "," ~ list <~ ")." ^^ {
      case ident ~ _ ~ list => defs get ident match {
        case Some(_) => throw new scala.Error("Set " + ident + " already defined")
        case None =>
          defs += (ident -> new NamedRangeSetUnion(ident, normal(list map defs)))
      }
    }
  }



  def hideEnd(rs : RangeSet) : Parser[Unit] = ("," ~>
    rangeListOrRange ~ "," ~
    rangeListOrRange ~ "," ~
    rangeListOrRange <~ ")." ^^ {
    case facades ~ _ ~ interlopers ~ _ ~ friends =>
      builder.addHideConstraint(rs, toDef(facades), toDef(interlopers), toDef(friends))
  }
    | ")." ^^ { case s =>
    builder.addHideConstraint(rs, LiteralRangeSet(),
      LiteralRangeSet(Scope(DependencyGraph.rootId)), LiteralRangeSet())})


  def hide : Parser[Unit] =
    "hide(" ~> range >> { r => hideEnd(LiteralRangeSet(findNode(r)))}

  def hideSet : Parser[Unit] =
    "hideSet(" ~> rangeListOrRange >> { s => hideEnd(toDef(s))}


  type Add2ArgsHideConstraint = (RangeSet, RangeSet) => Unit

  def addHideFromConstraint : Add2ArgsHideConstraint =
    (owner, interlopers) =>
      builder.addHideConstraint(owner,
        LiteralRangeSet(), interlopers, LiteralRangeSet())

  def addHideButFromConstraint : Add2ArgsHideConstraint =
    (owner, friends) =>
      builder.addHideConstraint(owner, LiteralRangeSet(),
        LiteralRangeSet(Scope(DependencyGraph.rootId)), friends)


  def hide2ArgsEnd(add : Add2ArgsHideConstraint)(rs : RangeSet) : Parser[Unit] =
    "," ~> rangeListOrRange <~ ")." ^^ { rl => add(rs, toDef(rl))}

  def hideFrom : Parser[Unit] =
    "hideFrom(" ~> range >> {r =>
      hide2ArgsEnd(addHideFromConstraint)(LiteralRangeSet(findNode(r)))
    }

  def hideSetFrom : Parser[Unit] =
    "hideSetFrom(" ~> rangeListOrRange >> {s =>
      hide2ArgsEnd(addHideFromConstraint)(toDef(s))
    }

  def hideButFrom : Parser[Unit] =
    "hideButFrom(" ~> range >> { r =>
      hide2ArgsEnd(addHideButFromConstraint)(LiteralRangeSet(findNode(r)))
    }

  def hideSetButFrom : Parser[Unit] =
    "hideSetButFrom(" ~> rangeListOrRange >> {s =>
      hide2ArgsEnd(addHideButFromConstraint)(toDef(s))
    }

  def hideFromEachOther : Parser[Unit] = {
    "hideFromEachOther(" ~> rangeListOrRange <~ ")." ^^ {
      case s =>
        val owners = toDef(s)
        builder.addHideConstraint(owners, LiteralRangeSet(),
          owners, LiteralRangeSet())
    }
  }

  def friend : Parser[Unit] =
    "friendOf(" ~> rangeListOrRange ~ "," ~ rangeListOrRange <~ ")." ^^ {
      case friends ~ _ ~ befriended =>
        builder.addFriendConstraint(toDef(friends), toDef(befriended))
    }

  def constraints : Parser[Unit] = {
    ( java_import
      | declare_set
      | declare_set_union
      | hide
      | hideFrom
      | hideButFrom
      | hideSet
      | hideSetFrom
      | hideSetButFrom
      | hideFromEachOther
      | friend
      )}

  /*def apply(input : java.io.Reader) = parseAll(constraints, input) match{
    case Success(result, _ ) => result
    case failure : NoSuccess => throw new scala.Error(failure.msg)
  }*/

  def apply(input : java.io.Reader) = {
    def aux(input : Reader[Char]) : Unit = {
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
    builder.setDefs(defs)
  }
}
