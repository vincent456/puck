package puck.graph
package constraints

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Reader, StreamReader}


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


  var defs : Map[String, NamedNodeSet] = Map()


  var imports : Seq[String] = Seq("")

  def findNode(k : String) : Seq[NodeId] ={

    def aux(imports : Seq[String], acc : Seq[NodeId]) : Seq[NodeId] =
      imports match {
        case Seq() if acc.isEmpty => throw new NoSuchElementException("named element " + k + " not found")
        case Seq() => acc
        case _ =>
          builder.nodesByName get (imports.head + k) match {
            case None => aux(imports.tail, acc)
            case Some(n) => aux(imports.tail, n +: acc)
          }
      }

    aux(imports, Seq())
  }

  def toDef (request : Either[String, Seq[String]]) : NodeSet = {
    request match {
      case Left(key) => defs get key match {
        case Some(l) => l
        case None => LiteralNodeSet(findNode(key))
      }
      case Right(l) => LiteralNodeSet(l flatMap findNode)
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
    "java_import(" ~> list <~ ")." ^^ { l : List[String] =>
      imports = l.foldLeft(imports) {case (acc, str) => (str +".") +: acc}
    }


  def declare_set : Parser[Unit] =
    "declareSet(" ~> ident ~ "," ~ list <~ ")." ^^ {
      case ident ~ _ ~ list => defs get ident match {
        case Some(_) => throw new scala.Error("Set " + ident + " already defined")
        case None => defs += (ident -> new NamedNodeSet(ident,
          LiteralNodeSet(list flatMap findNode)))
      }
    }

  def declare_set_union : Parser[Unit] = {


    def normal(list : Seq[NodeSet]) = {

      val (namedSets, lit) = list.foldLeft(( Seq[NodeSet](),LiteralNodeSet())){
        case ((nsAcc, litAcc), l : LiteralNodeSet) => (nsAcc, litAcc ++ l)
        case ((nsAcc, litAcc), ns) => (ns +: nsAcc, litAcc)
      }

      new NodeSetUnion(namedSets, lit)
    }

    "declareSetUnion(" ~> ident ~ "," ~ list <~ ")." ^^ {
      case ident ~ _ ~ list => defs get ident match {
        case Some(_) => throw new scala.Error("Set " + ident + " already defined")
        case None => defs += (ident -> new NamedNodeSetUnion(ident, normal(list map defs)))
      }
    }
  }



  def hideScope : Parser[Unit] =
    "hideScope(" ~> ident ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent <~ ")." ^^ {
      case  i ~ _ ~ facades ~ _ ~ interlopers ~ _ ~ friends =>
        builder.addScopeConstraint(LiteralNodeSet(findNode(i)),
          toDef(facades), toDef(interlopers), toDef(friends))
    }

  def hideScopeSet1 : Parser[Unit] =
    "hideScopeSet(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        builder.addScopeConstraint(toDef(s), LiteralNodeSet(),
          LiteralNodeSet(DependencyGraph.rootId), LiteralNodeSet())
    }

  def hideScopeSet4 : Parser[Unit] =
    "hideScopeSet(" ~> listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent ~ "," ~
      listOrIdent <~ ")." ^^ {
      case s ~ _ ~ facades ~ _ ~ interlopers ~ _ ~ friends =>
        builder.addScopeConstraint(toDef(s), toDef(facades),
          toDef(interlopers), toDef(friends))
    }

  def hideScopeFrom : Parser[Unit] =
    "hideScopeFrom(" ~> ident ~ "," ~ listOrIdent <~ ")." ^^ {
      case  i ~ _ ~  friends =>
        builder.addScopeConstraint(LiteralNodeSet(findNode(i)),
          LiteralNodeSet(), LiteralNodeSet(), toDef(friends))
    }

  def hideScopeSetFrom : Parser[Unit] =
    "hideScopeSetFrom(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case s ~ _ ~ interlopers =>
        builder.addScopeConstraint(toDef(s), LiteralNodeSet(),
          toDef(interlopers), LiteralNodeSet())
    }

  def hideScopeButFrom : Parser[Unit] =
    "hideScopeButFrom(" ~> ident ~ "," ~ listOrIdent <~ ")." ^^ {
      case i ~ _ ~ friends =>
        builder.addScopeConstraint(LiteralNodeSet(findNode(i)), LiteralNodeSet(),
          LiteralNodeSet(DependencyGraph.rootId), toDef(friends))
    }

  def hideScopeSetButFrom : Parser[Unit] =
    "hideScopeSetButFrom(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case s ~ _ ~ friends =>
        builder.addScopeConstraint(toDef(s), LiteralNodeSet(),
          LiteralNodeSet(DependencyGraph.rootId), toDef(friends))
    }

  def hideScopeFromEachOther : Parser[Unit] = {
    "hideScopeFromEachOther(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        val owners = toDef(s)
        builder.addScopeConstraint(owners, LiteralNodeSet(),
          owners, LiteralNodeSet())
    }
  }

  def friendsOfScope : Parser[Unit] =
    "friendOfScope(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case friends ~ _ ~ befriended =>
        builder.addScopeFriendOfScopeConstraint(toDef(friends), toDef(befriended))
    }

  def friendsOfElement : Parser[Unit] =
    "friendOfElement(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case friends ~ _ ~ befriended =>
        builder.addScopeFriendOfElementConstraint(toDef(friends), toDef(befriended))
    }

  def canSee  : Parser[Unit] =
    "canSee(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case friends ~ _ ~ befriended =>
        builder.addCanSee(toDef(friends), toDef(befriended))
    }


  def hideElement3 : Parser[Unit] =
    "hide(" ~> ident ~ "," ~ listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case elt ~ _ ~ interlopers ~ _ ~ friends =>
        builder.addElementConstraint(LiteralNodeSet(findNode(elt)),
          toDef(interlopers), toDef(friends))
    }

  def hideElementSet1 : Parser[Unit] =
    "hideSet(" ~> listOrIdent <~ ")." ^^ {
      case s =>
        builder.addElementConstraint(toDef(s),
          LiteralNodeSet(DependencyGraph.rootId), LiteralNodeSet())
    }

  def hideElementSet3 : Parser[Unit] =
    "hideSet(" ~> listOrIdent ~ "," ~ listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case set ~ _ ~ interlopers ~ _ ~ friends =>
        builder.addElementConstraint(toDef(set),
          toDef(interlopers), toDef(friends))
  }

  def hideElementFrom : Parser[Unit] =
    "hideFrom(" ~> ident ~ "," ~ listOrIdent <~ ")." ^^ {
    case elt ~ _ ~ interlopers =>
      builder.addElementConstraint(LiteralNodeSet(findNode(elt)),
        toDef(interlopers), LiteralNodeSet())
  }

  def hideElementSetFrom : Parser[Unit] =
    "hideSetFrom(" ~> listOrIdent ~ "," ~ listOrIdent <~ ")." ^^ {
      case s ~ _ ~ interlopers =>
        builder.addElementConstraint(toDef(s),
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
      | hideScopeFrom
      | hideScopeButFrom
      | hideScopeSetFrom
      | hideScopeSetButFrom
      | hideScopeFromEachOther
      | friendsOfScope
      | friendsOfElement
      | canSee
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
    builder.setDefs(defs)
  }
}
