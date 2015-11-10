/*
package puck.graph.transformations

import puck.graph.DGEdge
import puck.util.{PuckLog, PuckLogger}

object FilterNoise {

  implicit val defaultVerbosity = (PuckLog.NoSpecialContext, PuckLog.Debug)

/*  private def select[T](l : List[T], pred : T => Boolean) : (Option[T], List[T]) = {
    def aux(l1 : List[T], acc : List[T]) : (Option[T], List[T]) =
      if(l1.isEmpty) (None, l)
      else if(pred(l1.head)) (Some(l1.head), acc reverse_::: l1.tail)
      else aux(l1.tail, l1.head :: acc)

    aux(l, List[T]())
  }*/

  def writeRule(logger : PuckLogger)(name : => String, op1 : => String, op2 : => String, res : => String ) : Unit = {
    logger.writeln(name +" : ")
    logger.writeln(op1)
    logger.writeln("+ " + op2)
    logger.writeln("= " + res)
    logger.writeln("----------------------------------------------")
  }

  def mapUntil( stoppingEdge : DGEdge,
                transfos : List[Transformation],
                rule : (Transformation => Option[Transformation])): Seq[Transformation] = {

    def aux(acc : Seq[Transformation], l : Seq[Transformation]) : Seq[Transformation] ={
      if(l.isEmpty) acc.reverse
      else
        l.head match {
          case Transformation(_, RedirectionOp(`stoppingEdge`, _)) =>
            RecordingComparator.revAppend(acc, l)
          case _ => rule(l.head) match {
            case None => aux(acc, l.tail)
            case Some(t) => aux(t +: acc, l.tail)
          }
        }
    }
    aux(List(), transfos)
  }

  def xRedRule(logger : PuckLogger, red: RedirectionOp) : Transformation => Option[Transformation] = {
    val kind = red.edge.kind
    val n1 = red.edge.source
    val n2 = red.edge.target
    red.extremity match {
      case Target(n3) => {
        case op2@Transformation(Regular, RedirectionOp(e @ DGEdge(`kind`, `n1`, n0), Target(`n2`))) =>
          val res = Transformation(Regular, RedirectionOp(kind(n1, n0), Target(n3)))
          writeRule(logger)("RedRed_tgt", op2.toString, redTransfo(red).toString, res.toString)
          Some(res)
        case op2@Transformation(Regular, Edge(e @ DGEdge(`kind`, `n1`, `n2`))) =>
          val res = if(red.withMerge) None
          else Some(Transformation(Regular, Edge(kind(n1, n3))))
          writeRule(logger)("AddRed_tgt", op2.toString, redTransfo(red).toString, res.toString)
          res
        case t => Some(t)
      }
      case Source(n3) => {
        case op2@Transformation(Regular, RedirectionOp(e @ DGEdge(`kind`, n0, `n2`), Source(`n1`))) =>
          val res = Transformation(Regular, RedirectionOp(kind(n0, n2), Source(n3)))
          writeRule(logger)("RedRed_src", op2.toString, redTransfo(red).toString, res.toString)
          Some(res)
        case op2@Transformation(Regular, Edge(e @ DGEdge(`kind`, `n1`, `n2`))) =>
          val res = Transformation(Regular, Edge(kind(n3, n2)))
          writeRule(logger)("AddRed_src", op2.toString, redTransfo(red).toString, res.toString)
          Some(res)
        case t => Some(t)
      }
    }
  }

  def redTransfo : RedirectionOp => Transformation = Transformation(Regular, _)

/*
  def apply(transfos : Seq[Transformation], logger : PuckLogger): Seq[Transformation] = {
    val ts = filter(transfos, logger)
    if(ts.length == transfos.length) ts
    else apply(ts, logger)
  }
*/

  def apply(transfos : Seq[Transformation], logger : PuckLogger): Seq[Transformation] ={

    // We are going backwards !
    def aux(filteredTransfos : Seq[Transformation],
            l : Seq[Transformation],
            removedEdges : Seq[DGEdge]): (Seq[Transformation], Seq[DGEdge]) = {
      l match {
        case List() => (filteredTransfos, removedEdges)
/*        case (op2 @ Transformation(Remove, TTEdge(DGEdge(Contains, n1, n2)))) :: tl =>
          select[Transformation](tl,
          { case Transformation(Add, TTEdge(DGEdge(Contains, `n1`, `n2`))) => true
          case _ => false
          }) match {
            case (Some( op1 ), newTl) =>
              writeRule(logger)("AddDel", op1.toString, op2.toString, "None")
              aux(filteredTransfos, newTl, removedEdges)
            case (None, _) => aux(l.head +: filteredTransfos, l.tail, removedEdges)
          }*/

        case (op1 @ Transformation(Regular, red @ RedirectionOp(stopingEdge, _))) :: tl =>
          val transfos = mapUntil(stopingEdge, tl, xRedRule(logger, red))
          aux(filteredTransfos, transfos, removedEdges)


        case (t @ Transformation(Regular, Edge(_))):: tl => aux(t +: filteredTransfos, tl, removedEdges)
        case (Transformation(Reverse, Edge(e))):: tl => aux(filteredTransfos, tl, e +: removedEdges)

        case hd :: _ => sys.error(hd  + " should not be in list")
      }
    }

    val (normalTransfos, removedEdges) = aux(Seq(), transfos, Seq())

    removedEdges.foldLeft(normalTransfos){(normalTransfos0, e) =>
      logger.writeln(s"$e was removed filtering:")
      normalTransfos0 filter {
        case Transformation(Regular, Edge(`e`)) =>
          logger.writeln(s"add found")
          false
        case _ => true
      }
    }

  }
}
*/
