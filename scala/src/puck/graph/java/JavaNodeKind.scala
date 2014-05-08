package puck.graph.java

import puck.graph.{AGNode, DotHelper, NodeKind}
/**
 * Created by lorilan on 06/05/14.
 */

object JavaNodeKind extends DotHelper{
  case class Package private[JavaNodeKind]() extends NodeKind  //unused in core LJ
  case class Interface private[JavaNodeKind]() extends NodeKind //unused in LJ

  case class Class private[JavaNodeKind]() extends NodeKind
  case class Constructor private[JavaNodeKind]() extends NodeKind
  case class Method private[JavaNodeKind]() extends NodeKind
  case class Field private[JavaNodeKind]() extends NodeKind

  case class Literal private[JavaNodeKind]() extends NodeKind

  val `package` = new Package()

  val interface = new Interface()
  val `class` = new Class()

  val constructor = new Constructor()
  val method = new Method()
  val field = new Field()

  val literal = new Literal()

  //fix for accessing the field in java
  val interfaceKind = interface
  val classKind = `class`

  def isDotSubgraph(k:NodeKind) = k match {case Package() => true; case _ => false}
  def isDotClass(k:NodeKind)= k match { case Class() | Interface() => true; case _ => false}
  def fillColor(k:NodeKind)= k match {
    case Package() => "#FF9933" //Orange
    case Interface() => "#FFFF99" // Light yellow
    case Class() | Constructor() => "#FFFF33" //Yellow
    case Method() | Field() => "#FFFFFF" //White
    case Literal() => "#CCFFCC" //Very Light green
    case _ => throw new Error("Unknown JavaNodeKind")
  }

  def namePrefix(k:NodeKind)= k match {
    case Package() => "&lt;&lt;package&gt;&gt; "
    case Interface() => "&lt;&lt;interface&gt;&gt; "
    case _ => ""
  }

  def splitDotClassContent(n: AGNode)={
      n.getContent.foldLeft( (List[AGNode](), List[AGNode](), List[AGNode](), List[AGNode]()) ){
        ( lists : (List[AGNode], List[AGNode], List[AGNode] , List[AGNode]), n : AGNode ) =>
         val (fds, cts, mts, cls) = lists
         n.kind match{
           case Interface() | Class() => (fds, cts, mts, n::cls)
           case Field() => (n::fds, cts, mts, cls)
           case Constructor() => (fds, n::cts, mts, cls)
           case Method() => (fds, cts, n::mts, cls)
           case _ => throw new Error("Wrong NodeKind contained by a class")
         }
      }


  }



}
