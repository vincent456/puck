package puck.graph

import puck.graph.backTrack.{Transformation, Recording, CareTakerNoop, CareTaker}
import puck.util.{PuckNoopLogger, PuckLogger, Logger, PuckLog}

import scala.language.implicitConversions
import scala.collection.mutable
import puck.graph.constraints._

/**
 * Created by lorilan on 05/05/14.
 */

object AccessGraph {
  val rootId = 0
  val rootName = "root"
  val unrootedStringId = "<DETACHED>"

  implicit def agToIterator[Kind <: NodeKind[Kind]](ag : AccessGraph[Kind]) : Iterator[AGNode[Kind]] = ag.iterator

}

class AccessGraph[Kind <: NodeKind[Kind]] (nodeBuilder : AGNodeBuilder[Kind]) {
  //extends Iterable[AGNode]{ //is NOT iterable (see scala doc for requirements) but
  // has a valid iterator + implicit conversion in puck.graph package object

  type NodeType = AGNode[Kind]
  type EdgeType = AGEdge[Kind]

  var logger : PuckLogger = PuckNoopLogger
  implicit val defaulVerbosity : PuckLog.Verbosity = (PuckLog.InGraph(), PuckLog.Debug())
  implicit def logVerbosity(lvl : PuckLog.Level) = (PuckLog.InGraph(), lvl)

  def newGraph() : AccessGraph[Kind] = {
    new AccessGraph(nodeBuilder)
  }

  logger.writeln("Node builder : " + nodeBuilder.getClass)(PuckLog.InGraph(), PuckLog.Debug())

  val nodeSets : mutable.Map[String, NamedNodeSet[Kind]] = mutable.Map()
  val constraints : mutable.Buffer[Constraint[Kind]] = mutable.Buffer()

  private [this] val nodes0 = mutable.Buffer[NodeType]()

  def nodes : Iterator[NodeType] = nodes0.iterator

  def iterator = root.iterator

  private var id : Int = 1
  val root : NodeType = nodeBuilder(this, AccessGraph.rootId, AccessGraph.rootName, nodeBuilder.rootKind)

  val scopeSeparator = nodeBuilder.scopeSeparator

  /*override def hashCode: Int = this.id * 42 // ???
  */

  def nodeKinds = nodeBuilder.kinds

  def violations : List[EdgeType] = {
    this.foldLeft(List[EdgeType]()){
      (acc: List[EdgeType], n :NodeType) =>
        n.wrongUsers.map{AGEdge.uses(_, n)} :::(
          if(n.isWronglyContained )
            AGEdge.contains(n.container, n) :: acc
          else acc)
    }
  }


  def discardConstraints() {
    nodeSets.clear()
    this.foreach(_.discardConstraints())
  }

  def printConstraints[V](logger : Logger[V], v : V){
    nodeSets.foreach{
      case (_, namedSet) => logger.writeln(namedSet.defString)(v)
    }
    constraints.foreach(ct => logger.writeln(ct)(v))
  }

  private [puck] val nodesByName = mutable.Map[String, NodeType]()

  def apply(fullName:String) : NodeType= nodesByName(fullName)
  def getNode(fullName:String) : Option[NodeType] = nodesByName get fullName


  def addNode(fullName: String, localName:String, kind: Kind): NodeType = {
    val unambiguousFullName = nodeBuilder.makeKey(fullName, localName, kind)
    nodesByName get unambiguousFullName match {
      case None =>
        val n = addNode(localName, kind)
        this.nodesByName += (unambiguousFullName -> n)
        n
      case Some(n) => n /* check that the kind and type is indeed the same ??*/
    }
  }

  def remove(n : NodeType){
    nodes0 -= n
    transformations.removeNode(n)
  }

  var transformations : CareTaker[Kind] = new CareTaker(this)
  var initialRecord : List[Transformation[Kind]] = _

  def apply(r : Recording[Kind]){
    transformations.recording.undo()
    transformations.recording = r
  }


  def addNode(n : NodeType) : NodeType = {
    //assert n.graph == this ?
    this.nodes0 += n
    transformations.addNode(n)
    n
  }

  def addNode(localName:String, kind: Kind) : NodeType = {
    id = id + 1
    val n = nodeBuilder(this, id, localName, kind.create())
    addNode(n)
    //this.root.content_+=(n)
    //n
  }

  /* a primary use is for example the use of a class when declaring a variable or a parameter
 a side use is in this example a call to a method with the same variable/parameter
    the use of the method is a side use of the declaration
     a couple primary use/ side use is a use dependency
*/

  /* keys are side uses and values ar primary uses */
  val primaryUses = new UsesDependencyMap[Kind](Dominated())

  /* keys are primary uses and values ar side uses */
  val sideUses = new UsesDependencyMap[Kind](Dominant())

  def addUsesDependency(primary : EdgeType, side : EdgeType){

    /*side.user.primaryUses get side.usee match {
      case None => ()
      case Some(s) => println("adding" + primary + " as primary uses of " + side + ": edge already has primary uses : " + s)
      //content += (usee -> s.+=(dependency))
    }*/

    sideUses += (primary, side)
    primaryUses += (side, primary)
    transformations.addEdgeDependency(primary, side)
  }
  def addUsesDependency(primaryUser : NodeType, primaryUsee : NodeType,
                        sideUser : NodeType, sideUsee : NodeType) {
    addUsesDependency(AGEdge.uses(primaryUser, primaryUsee),
      AGEdge.uses(sideUser, sideUsee))
  }

  def removeUsesDependency(primary : EdgeType, side : EdgeType){
    sideUses -= (primary, side)
    primaryUses -= (side, primary)
    transformations.removeEdgeDependency(primary, side)
  }

  def removeUsesDependency(primaryUser : NodeType, primaryUsee : NodeType,
                           sideUser : NodeType, sideUsee : NodeType) {
    removeUsesDependency(AGEdge.uses(primaryUser, primaryUsee),
      AGEdge.uses(sideUser, sideUsee))
  }


  def applyChangeOnProgram(record : Recording[Kind]){}

  def coupling = this.foldLeft(0 : Double){ (acc, n) => acc + n.coupling }


  def redirectUses(oldEdge : EdgeType, newUsee : NodeType,
                   policy : RedirectionPolicy,
                   propagateRedirection : Boolean = true,
                   keepOldUse : Boolean = false ) = {
    if(oldEdge.usee == newUsee) oldEdge
    else if(oldEdge.exists) {

      logger.writeln("redirecting %s target to %s (%s)".format(oldEdge, newUsee, policy))

      val newUses =
          if(keepOldUse) {
            val newUse = AGEdge.uses(oldEdge.user, newUsee)
            newUse.create()
            newUse
          }
          else
            oldEdge.changeTarget(newUsee)

      oldEdge.user.kind match {
        case k : HasType[Kind, _] => k.redirectUses(oldEdge.usee, newUsee)
        case _ => ()
      }

      if(propagateRedirection) {
        redirectPrimaryUses(oldEdge, newUsee, policy)
        redirectSideUses(oldEdge, newUsee, policy)
      }

      newUses
    }
    else if (oldEdge.user uses newUsee) {
      //if m uses  both A and A.mi, the first uses dominate the second
      //if both are identified as violations and are in a wrongusers list
      //redirecting the one will redirect the other
      // when iterating on the wrongusers, the next call to redirectuses will arrive here
      logger.writeln("redirecting uses' %s target to %s (%s) : FAILURE !! %s is not used".
        format(oldEdge, newUsee, policy, oldEdge.usee))
      AGEdge.uses(oldEdge.user, newUsee)
    }
    else {
      if(oldEdge.usee.users.contains(oldEdge.user) || newUsee.users.contains(oldEdge.user))
        throw new AGError("incoherent state !!!!!!!!!!!!")

      throw new AGError(("redirecting uses' %s target to %s (%s)\n" +
        "!!! nor the oldUsee or the newUsee is really used !!! ").
        format(oldEdge, newUsee, policy))
    }
  }

  def redirectPrimaryUses(currentSideUse : EdgeType,
                          newSideUsee : NodeType,
                          policy : RedirectionPolicy,
                          propagateRedirection : Boolean = true){


    logger.writeln("redirecting primary uses of side use %s (new side usee is %s) ".
      format(currentSideUse, newSideUsee))

    primaryUses get currentSideUse match {
      case None =>
        logger.writeln("no primary uses to redirect")

      case Some(primary_uses) =>
        assert(primary_uses.nonEmpty)

        logger.writeln("uses to redirect:%s".format(primary_uses.mkString("\n\t", "\n\t","\n")))



        primary_uses foreach {
          primary =>
            removeUsesDependency(primary, currentSideUse)

            val keepOldUse = (sideUses get primary).nonEmpty //is empty if primary had only one side use

            val newPrimary =
              redirectUses(primary, findNewPrimaryUsee(primary.usee, newSideUsee, policy),
              policy, propagateRedirection, keepOldUse)


            addUsesDependency(newPrimary, AGEdge.uses(currentSideUse.user, newSideUsee))
        }
    }

  }

  def findNewPrimaryUsee(currentPrimaryUsee : NodeType,
                         newSideUsee : NodeType,
                         policy : RedirectionPolicy) : NodeType = {

    logger.writeln("searching new primary usee ("+ policy + ") : currentPrimaryUsee is " +
      currentPrimaryUsee + ", new side usee " + newSideUsee)

    val newPrimaryUsee =
      policy match {
        case Move() => newSideUsee.container
        case absPolicy : AbstractionPolicy =>
          currentPrimaryUsee.abstractions.find{
            case (node, `absPolicy`) => node.contains_*(newSideUsee)
            case _ => false
          } match {
            case Some((n, _)) => n
            case None =>
              iterator.find{ node =>
                node.contains_*(newSideUsee) &&
                  currentPrimaryUsee.kind.abstractKinds(absPolicy).contains(node.kind)
              } match {
                case Some(n) => n
                case None =>
                  val msg = "no correct primary abstraction found !"
                  logger.writeln(msg)(PuckLog.Error())
                  throw new RedirectionError(msg)
              }
          }
      }
    logger.writeln("new primary usee found : " + newPrimaryUsee)
    newPrimaryUsee
  }


  def redirectSideUses(currentPrimaryUse: EdgeType,
                       newPrimaryUsee : NodeType,
                       policy : RedirectionPolicy){
    logger.writeln("redirecting side uses of primary use %s (new primary usee is %s) ".
      format(currentPrimaryUse, newPrimaryUsee))

    sideUses get currentPrimaryUse match {
      case None =>
        logger.writeln("no side uses to redirect")
        ()
      case Some( sides_uses ) =>
        logger.writeln("uses to redirect:%s".format(sides_uses.mkString("\n\t", "\n\t","\n")))

        sides_uses foreach {
          side =>
            side.usee.abstractions.find {
              case (abs, _) => newPrimaryUsee.contains(abs)
              case _ => false
            } match {
              case None =>
                val msg = ("While redirecting primary uses %s target to %s\n" +
                  "no satisfying abstraction to redirect side use %s").
                  format(currentPrimaryUse, newPrimaryUsee, side)
                logger.writeln(msg)(PuckLog.Error())

                throw new RedirectionError(msg)

              case Some( (new_side_usee, _) ) =>

                removeUsesDependency(currentPrimaryUse, side)

                val newSide = redirectUses(side, new_side_usee, policy)

                addUsesDependency(AGEdge.uses(currentPrimaryUse.user, newPrimaryUsee), newSide)

            }
        }
    }
  }

}

