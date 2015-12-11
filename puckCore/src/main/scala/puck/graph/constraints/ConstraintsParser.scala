package puck.graph
package constraints


import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Reader, StreamReader}
import puck.graph.constraints.{Keywords => KW}

case class ConstraintMapBuilder
(nodesByName : Map[String, NodeId],
 imports : Seq[String] = Seq(""),
 defs : Map[String, NamedRangeSet] = Map(),
 friendCtsMap : Map [Range, ConstraintSet] = Map(),
 hideConstraintsMap : Map[Range, ConstraintSet] = Map()){
  def addImport(iprt : String) : ConstraintMapBuilder =
    copy(imports = (iprt + ".") +: imports)
  //appended dot needed for import resolution

  def addDef(nrs : NamedRangeSet) : ConstraintMapBuilder =
      defs get nrs.id match {
        case Some(_) =>
          throw new scala.Error(s"Set ${nrs.id} already defined")
        case None => copy(defs =  defs + (nrs.id -> nrs))
      }


  def findNode(k : ParsedId) : Seq[Range] ={

    def aux(imports : Seq[String], acc : Seq[Range]) : Seq[Range] =
      imports match {
        case Seq() if acc.isEmpty => throw new NoSuchElementException("named element " + k + " not found")
        case Seq() => acc
        case _ =>
          nodesByName get (imports.head + k.node) match {
            case None => aux(imports.tail, acc)
            case Some(n) => aux(imports.tail, k.range(n) +: acc)
          }
      }

    aux(imports, Seq())
  }

  def getDef : Either[ParsedId, Seq[ParsedId]] => RangeSet = {
    case Left(key) => defs get key.node match {
      case Some(l) => key.range match {
        case RangeBuilder.Scope => l
        case RangeBuilder.Element => RootedRangeSet(l)
      }
      case None => LiteralRangeSet(findNode(key))
    }
    case Right(l) => LiteralRangeSet(l flatMap findNode)
  }

  import ConstraintsMaps.addConstraintToMap

  def addHideConstraint
  ( owners : RangeSet,
    facades : RangeSet,
    interlopers : RangeSet,
    friends : RangeSet
    ) : ConstraintMapBuilder = {

    val ct = new Constraint(owners, facades, interlopers, friends)

    copy(hideConstraintsMap = addConstraintToMap(hideConstraintsMap, ct))

  }

  def addFriendConstraint
  ( friends : RangeSet,
    befriended : RangeSet
    ) : ConstraintMapBuilder = {

      val ct =
        new Constraint(befriended, RangeSet.empty(), RangeSet.empty(), friends)

      copy(friendCtsMap = addConstraintToMap(friendCtsMap, ct))

  }
}


object ConstraintsParser
  extends RegexParsers {

  protected override val whiteSpace = """(\s|%.*|#.*)+""".r  //to skip comments

  //val eol = ";"

  def ident : Parser[String] = (
    """[^\[\]\s',().=;]+""".r
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

  def importClause(cm : ConstraintMapBuilder) : Parser[ConstraintMapBuilder] =
    "import" ~> list ^^ { l : List[String] =>
      l.foldLeft(cm) {case (acc, str) => acc.addImport(str)}
    }

  def declaration(cm : ConstraintMapBuilder) : Parser[ConstraintMapBuilder] =
    ident <~ "=" >> declarationEnd(cm)

  private def normalize(list : Seq[RangeSet]) = {
    val (namedSets, lit) = list.foldLeft(( Seq[RangeSet](),LiteralRangeSet())){
      case ((nsAcc, litAcc), l : LiteralRangeSet) => (nsAcc, litAcc ++ l)
      case ((nsAcc, litAcc), ns) => (ns +: nsAcc, litAcc)
    }
    new RangeSetUnion(namedSets, lit)
  }
  
  def declarationEnd
  ( cm : ConstraintMapBuilder)
  ( ident : String ) : Parser[ConstraintMapBuilder] =
    ( "union(" ~> list <~ ")" ^^ {
        list =>
         cm.addDef( new NamedRangeSetUnion(ident, normalize(list map cm.defs)))
      }
      | rangeList ^^ {
       list => cm.addDef(new NamedRangeSet(ident,
        LiteralRangeSet(list flatMap cm.findNode)))
    })


  def hide(cm : ConstraintMapBuilder) : Parser[RangeSet] =
    KW.hide ~> rangeListOrRange ^^ cm.getDef

  def except(cm : ConstraintMapBuilder) : Parser[RangeSet] =
    KW.except ~> rangeListOrRange ^^ cm.getDef

  def from(cm : ConstraintMapBuilder) : Parser[RangeSet] =
    KW.from ~> rangeListOrRange ^^ cm.getDef

  def butNotFrom(cm : ConstraintMapBuilder) : Parser[RangeSet] =
    KW.butNotFrom ~> rangeListOrRange ^^ cm.getDef

  def hideCt
  ( cm : ConstraintMapBuilder ) : Parser[ConstraintMapBuilder] =
    hide(cm) >> exceptEnd(cm)

  def exceptEnd
  ( cm : ConstraintMapBuilder )
  ( ctOwner : RangeSet ) : Parser[ConstraintMapBuilder] =
    (except(cm) | success(LiteralRangeSet())) >>
      fromEnd(cm, ctOwner)

  def fromEverything : Parser[RangeSet] =
    success(LiteralRangeSet(Scope(DependencyGraph.rootId)))

  def fromEnd
  ( cm : ConstraintMapBuilder,
    ctOwner : RangeSet)
  ( facades : RangeSet) : Parser[ConstraintMapBuilder] =
    (from(cm) | fromEverything ) >>
      butNotFromEnd(cm, ctOwner, facades)

  def butNotFromEnd
  ( cm : ConstraintMapBuilder,
    ctOwner : RangeSet,
    facades : RangeSet)
  ( interlopers : RangeSet) : Parser[ConstraintMapBuilder] =
    ( butNotFrom(cm)
      | success(LiteralRangeSet())) ^^ {
      friends =>
      cm.addHideConstraint(ctOwner,
        facades,
        interlopers,
        friends)
    }

  def friend(cm : ConstraintMapBuilder) : Parser[ConstraintMapBuilder] =
    rangeListOrRange ~ KW.friendOf ~ rangeListOrRange ^^ {
      case friends ~ _ ~ befriended =>
        cm.addFriendConstraint(cm getDef friends, cm getDef befriended)
    }

  def constraints(cm : ConstraintMapBuilder) : Parser[ConstraintMapBuilder] = {
    ( importClause(cm)
      | hideCt(cm)
      | declaration(cm)
      | friend(cm)
      )}

  def apply
  ( nodesByName : Map[String, NodeId],
    input : java.io.Reader
    ) : ConstraintsMaps = {
    def aux( cmIn : ConstraintMapBuilder, input : Reader[Char]) : ConstraintMapBuilder = {
      parse(constraints(cmIn), input) match {
        case Success(cmOut, i) => aux(cmOut, i)
        case Error(msg, next) =>
          throw new scala.Error("!!! Error !!! at position " + next.pos + " " + msg)
        case Failure(msg, next) =>
          if (next.atEnd) cmIn
          else throw new scala.Error("Failure at position " + next.pos + " " + msg)
      }
    }
    val builder = aux(ConstraintMapBuilder(nodesByName), StreamReader(input))
    ConstraintsMaps(builder.defs,
      builder.friendCtsMap,
      builder.hideConstraintsMap)
  }
}
