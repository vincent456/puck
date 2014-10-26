package puck.graph.backTrack.comparison

import puck.graph._
import puck.graph.backTrack._

/**
 * Created by lorilan on 01/10/14.
 */
class SimplificationRules[Kind <: NodeKind[Kind]] {
  //[Kind <: NodeKind[Kind]] {

  def otherMemberPred(t: Transformation[Kind]): Option[(Transformation[Kind]) => Boolean] = t.target match {

    case TTEdge(AGEdge(Contains(), n1, n2)) =>
      Some({case Transformation(op, TTEdge(AGEdge(Contains(), `n1`, `n2`)))
        if op == t.operation.reverse => true
      case _ => false})

    case TTEdge(AGEdge(kind, n1, n2)) if t.operation == Add() =>
      Some({case Transformation(Add(), TTRedirection(AGEdge(`kind`, `n1`, `n2`), _)) => true
      case _ => false })

    case TTRedirection(AGEdge(kind, n1, n2), Target(n3)) =>
      Some({case Transformation(Add(), TTEdge(AGEdge(`kind`, `n1`, `n2`))) => true
      case Transformation(Add(), TTRedirection(AGEdge(`kind`, `n1`, `n3`), Target(_))) => true
      case Transformation(Add(), TTRedirection(AGEdge(`kind`, `n1`, _), Target(`n2`))) => true
      case _ => false})


    case TTRedirection(AGEdge(kind, n1, n2), Source(n3)) =>
      Some({case Transformation(Add(), TTEdge(AGEdge(`kind`, `n1`, `n2`))) => true
      case Transformation(Add(), TTRedirection(AGEdge(`kind`, `n3`, `n2`), Source(_))) => true
      case Transformation(Add(), TTRedirection(AGEdge(`kind`, _, `n2`), Source(`n1`))) => true
      case _ => false})

    case _ => None
  }

  def apply(t1: Transformation[Kind], t2: Transformation[Kind]): Option[Transformation[Kind]] = {
    //hypothesis : t1 and t2 were identified using otherMemberPred

    def wrap(tgt : TransformationTarget[Kind]) = Some(Transformation(Add(), tgt))

    def aux (t1: Transformation[Kind], t2: Transformation[Kind]) = t1.target match {
      //because of hypothesis no need to test op (or nodes in the first case)
      case TTEdge(AGEdge(Contains(), _, _)) => None

      case TTEdge(AGEdge(kind, n1, n2)) =>
        t2.target match {
          case TTRedirection(_, Source(n3)) => wrap(TTEdge(kind(n3,n2)))
          case TTRedirection(_, Target(n3)) => wrap(TTEdge(kind(n1,n3)))
          case _ => sys.error("does not match hypothesis")
        }

      case TTRedirection(AGEdge(kind, n1, n2), Target(n3)) =>
        t2.target match {
          case TTEdge(AGEdge(`kind`, `n1`, `n2`)) => wrap(TTEdge(kind(n1, n3)))

          case TTRedirection(AGEdge(`kind`, `n1`, `n3`), Target(n4)) =>
            wrap(TTRedirection(kind(n1, n2), Target(n4)))

          case TTRedirection(AGEdge(`kind`, `n1`, n4), Target(`n2`)) =>
            wrap(TTRedirection(kind(n1, n4), Target(n3)))

          case _ => sys.error("does not match hypothesis")
        }


      case TTRedirection(AGEdge(kind, n1, n2), Source(n3)) =>
        t2.target match {
          case TTEdge(AGEdge(`kind`, `n1`, `n2`)) => wrap(TTEdge(kind(n3, n2)))

          case TTRedirection(AGEdge(`kind`, `n3`, `n2`), Source(n4)) =>
              wrap(TTRedirection(kind(n1, n2), Source(n4)))

          case TTRedirection(AGEdge(`kind`, n4, `n2`), Source(`n1`)) =>
              wrap(TTRedirection(kind(n4, n2), Source(n3)))

          case _ => sys.error("does not match hypothesis")
        }


      case _ => sys.error("does not match hypothesis")
        //throw new CannotApplySimplificationRule()
    }

    println("------------------------------------------------------")
    println(t1)
    println("+ " + t2)
    val res = aux(t1, t2)
      /*try { aux(t1, t2)
    } catch {
      case e : CannotApplySimplificationRule =>
        aux(t2, t1)
    }*/
    println("= " + res)
    res
  }


}



/*
class SimplificationRules[Kind <: NodeKind[Kind]] (val nodesToRemove : Set[AGNode[Kind]]){
  //[Kind <: NodeKind[Kind]] {

  def containedByMoreThanOne(n0 : AGNode[Kind], n1 : AGNode[Kind], n2 : AGNode[Kind]) : Boolean = {
    var cpt = 0
    if(nodesToRemove.contains(n0))
      cpt += 1

    if(nodesToRemove.contains(n1))
      cpt += 1

    if(nodesToRemove.contains(n2))
      cpt +=1

    cpt > 1
  }

  def otherMemberPred(t: Transformation[Kind]): Option[(Transformation[Kind]) => Boolean] = t.target match {

    //security if more than one Node is neuter do not know what to do
    // try a fix point
    /*case TTEdge(AGEdge(Uses(), src, tgt))
      if t.operation == Add() && neuterNodes.contains(tgt) && neuterNodes.contains(src) => ???
    case TTRedirection(AGEdge(Uses(), src, tgt), ext) if containedByMoreThanOne(src, tgt, ext.node) => ???
    */

    //case TTEdge(AGEdge(Contains(), n1, n2)) if neuterNodes.contains(n1) && neuterNodes.contains(n2) => ???
    case TTEdge(AGEdge(Contains(), n1, n2)) if nodesToRemove.contains(n1) || nodesToRemove.contains(n2) =>
      Some({case Transformation(op, TTEdge(AGEdge(Contains(), `n1`, `n2`)))
        if op == t.operation.reverse => true
      case _ => false})


    /* Uses rules  */
    //Pair #1 AddRedTgt
    case TTEdge(AGEdge(kind, n1, abs)) if t.operation == Add() && nodesToRemove.contains(abs) =>
      Some({case Transformation(Add(), TTRedirection(AGEdge(`kind`, `n1`, `abs`), Target(_))) => true
      case _ => false })

    case TTRedirection(AGEdge(kind, n1, abs), Target(_)) if nodesToRemove.contains(abs) =>
      Some({case Transformation(Add(), TTEdge(AGEdge(`kind`, `n1`, `abs`))) => true
      case _ => false})

    //Pair #2 AddRedSrc
    case TTEdge(AGEdge(Uses(), abs, n1)) if t.operation == Add() && nodesToRemove.contains(abs) =>
      Some({case Transformation(Add(), TTRedirection(AGEdge(Uses(), `abs`, `n1`), Source(_))) => true
      case _ => false })
    case TTRedirection(AGEdge(Uses(), abs, n1), Source(_)) if nodesToRemove.contains(abs) =>
      Some({case Transformation(Add(), TTEdge(AGEdge(Uses(), `abs`, `n1`))) => true
      case _ => false})

    //Pair #3 RedRedTgt
    case TTRedirection(AGEdge(Uses(), n1, _), Target(abs)) if nodesToRemove.contains(abs) =>
      Some({case Transformation(Add(), TTRedirection(AGEdge(Uses(), `n1`, `abs`), Target(_))) => true
      case _ => false})
    case TTRedirection(AGEdge(Uses(), n1, abs), Target(_)) if nodesToRemove.contains(abs) =>
      Some({case Transformation(Add(), TTRedirection(AGEdge(Uses(), `n1`, _), Target(`abs`))) => true
      case _ => false})

    //Pair #4 RedRedSrc
    case TTRedirection(AGEdge(Uses(), _, n1), Source(abs)) if nodesToRemove.contains(abs) =>
      Some({case Transformation(Add(), TTRedirection(AGEdge(Uses(), `abs`, `n1`), Source(_))) => true
      case _ => false})
    case TTRedirection(AGEdge(Uses(), abs, n1), Source(_)) if nodesToRemove.contains(abs) =>
      Some({case Transformation(Add(), TTRedirection(AGEdge(Uses(), _, `n1`), Source(`abs`))) => true
      case _ => false})


    case _ => None
  }

  def apply(t1: Transformation[Kind], t2: Transformation[Kind]): Option[Transformation[Kind]] = {
    //hypothesis : t1 and t2 were identified using otherMemberPred

    def aux (t1: Transformation[Kind], t2: Transformation[Kind]) = (t1, t2) match {
      case (Transformation(Add(), TTEdge(AGEdge(kind1, n11, abs1))),
      Transformation(Add(), TTRedirection(AGEdge(kind2, n12, abs2), Target(n2))))
        if kind1 == kind2 && n11 == n12 && abs1 == abs2 && nodesToRemove.contains(abs1) =>
        Some(Transformation(Add(), TTEdge(kind1(n11, n2))))

      case (Transformation(Add(), TTEdge(AGEdge(Uses(), abs1, n11))),
      Transformation(Add(), TTRedirection(AGEdge(Uses(), abs2, n12), Source(n0))))
        if n11 == n12 && abs1 == abs2 && nodesToRemove.contains(abs1) =>
        Some(Transformation(Add(), TTEdge(AGEdge.uses(n0, n11))))

      case(Transformation(Add(), TTRedirection(AGEdge(Uses(), n11, n0), Target(abs1))),
      Transformation(Add(), TTRedirection(AGEdge(Uses(), n12, abs2), Target(n2))))
        if n11 == n12 && abs1 == abs2 && nodesToRemove.contains(abs1) =>
        Some(Transformation(Add(), TTRedirection(AGEdge(Uses(), n11, n0), Target(n2))))

      case(Transformation(Add(), TTRedirection(AGEdge(Uses(), n1, n01), Source(abs1))),
      Transformation(Add(), TTRedirection(AGEdge(Uses(), abs2, n02), Source(n2))))
        if n01 == n02 && abs1 == abs2 && nodesToRemove.contains(abs1) =>
        Some(Transformation(Add(), TTRedirection(AGEdge(Uses(), n1, n01), Source(n2))))

      //because of hypothesis no need to test op or nodes
      case (Transformation(_, TTEdge(AGEdge(Contains(), _, _))),
      Transformation(_, TTEdge(AGEdge(Contains(), _, _)))) => None

      case _ => throw new CannotApplySimplificationRule()
    }

    println("------------------------------------------------------")
    println(t1)
    println("+ " + t2)
    val res = try { aux(t1, t2)
    } catch {
      case e : CannotApplySimplificationRule =>
        aux(t2, t1)
    }
    println("= " + res)
    res
  }


}
 */