package puck.graph
package constraints

import scalaz.Cord

object ShowConstraints {
  import ShowDG.CordBuilder

  implicit def rangeCord : CordBuilder[Range] = { (dg, r) =>
    val prefix = r match { case Scope(id) => "'" case Element(id) => "e:'" }
    Cord(prefix, dg.fullName(r.nid), "'")
  }

  def namedRangeSetDefCord : CordBuilder[NamedRangeSet] = (dg, nrs) =>
    Cord(nrs.declare, "(" + nrs.id + ", ", rangeSetCord(dg, nrs.setDef),").")

  implicit def rangeSetCord : CordBuilder[RangeSet] = { (dg, rs) =>
    rs match {
      case NamedRangeSet(id, setDef) => id
      case RangeSetUnion(sets, set) =>
        Cord("[", Cord.mkCord(",\n", sets.map(rangeSetCord(dg,_)).toSeq:_*), ",\n",
            Cord.mkCord(",\n", set.map(rangeCord(dg,_)).toSeq:_*), "]")
      case RangeSetDiff(plus, minus) =>
        Cord(rangeSetCord(dg, plus), "\\", rangeSetCord(dg, minus))
      case LiteralRangeSet(content) =>
        Cord("[", Cord.mkCord(",\n", content.map(rangeCord(dg,_)).toSeq:_*), "]")
    }
  }

  implicit def constraintCord : CordBuilder[Constraint] = {(dg, c) =>

    def twoArgsFormat(prefix : String, set : RangeSet) =
      Cord(prefix, "(", rangeSetCord(dg, c.owners), ", ", rangeSetCord(dg, set), ").")

    (c.facades.isEmpty, c.interlopers.isEmpty, c.friends.isEmpty) match {
      case (true, false, true) =>
        if(dg.isRoot(c.interlopers.head.nid))
          Cord("hide(", rangeSetCord(dg, c.owners), ").")
        else
          twoArgsFormat("hideFrom", c.interlopers)
      case (true, false, false)
        if dg.isRoot(c.interlopers.head.nid) => twoArgsFormat("hideButFrom", c.friends)
      case (false, false, true)
        if dg.isRoot(c.interlopers.head.nid) => twoArgsFormat("hideBut", c.facades)
      case (_, _, _) => Cord("hide(", rangeSetCord(dg, c.owners), ",\n",
        rangeSetCord(dg, c.facades), ",\n",
        rangeSetCord(dg, c.interlopers),  ",\n",
        rangeSetCord(dg, c.friends), ").")
    }
  }

  implicit def constraintSetCord :  CordBuilder[ConstraintSet] = { (dg, cs) =>
    Cord.mkCord("\n", cs.content.map(constraintCord(dg, _)).toSeq:_*)
  }
}
