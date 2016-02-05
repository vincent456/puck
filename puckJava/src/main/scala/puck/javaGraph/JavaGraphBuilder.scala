package puck.javaGraph

import java.util.NoSuchElementException

import puck.graph._
import puck.graph.constraints.{SupertypeAbstraction, ConstraintsMaps}
import puck.javaGraph.nodeKind._


import DependencyGraph._

import scalaz.{-\/, \/-}

trait JavaGraphBuilder extends GraphBuilder{

   g = new DependencyGraph(JavaNodeKind,
     NodeIndex(JavaNodeKind.root), EdgeMap(),
     AbstractionMap(), ConstraintsMaps(), Recording())



  val arrayTypeId = addNode("@primitive.[]","[]", GenericClass,  mutable = false)

  def addPackageNode(fullName: String, localName:String, mutable : Boolean) : NodeIdT =
    addNode(fullName, localName, Package, mutable)

  def getDefinition(nid : NodeId) =
     g.getConcreteNode(nid).definition_!(g)
//    catch {
//      case e :  NoSuchElementException =>
//        error(s"missing def for ${g.getNode(nid)}")
//    }


  def addPackage(p : String, mutable : Boolean): NodeIdT =
    nodesByName get p match {
      case None =>
        val fp = filterPackageName(p)
        val path = fp split "[.]"
        if (path.isEmpty) addPackageNode(fp, fp, mutable)
        else {
          val (_, n):(StringBuilder, NodeIdT) = path.foldLeft((new StringBuilder(), rootId)){
              case ((sb, nodeParent), p) =>
                sb append p
                val nId = addPackageNode(sb.toString(), p, mutable)
                addContains(nodeParent, nId)
                sb append "."
                (sb, nId)
          }
          n
        }
      case  Some(pn) => pn
    }


  override def registerAbstraction : DependencyGraph => (ImplId, Abstraction) => DependencyGraph =
    graph => (implId , abs) =>
      abs match {
        case AccessAbstraction(absId, SupertypeAbstraction) =>
          val impl = graph.getConcreteNode(implId)
          val absNode = graph.getConcreteNode(absId)
          (impl.kind, absNode.kind) match {
            /*case (Class, Class)
              | (Class, Interface)
              | (Interface, Interface) =>
            */
            case (Class, Interface) =>

              val absMeths = (graph content absId).toList filter (id => graph.kindType(id) == InstanceValueDecl) map graph.typedNode
              val candidates = (graph content impl.id).toList filter (id => graph.kindType(id) == InstanceValueDecl) map graph.typedNode
              Type.findAndRegisterOverridedInList(graph, absMeths, candidates) {
                Type.ignoreOnImplemNotFound
                  //errorOnImplemNotFound(graph.fullName(impl.id))
              } .value match {
                case \/-(g) => g.addAbstraction(implId, abs)
                case -\/(err) => throw err
              }

            case _ => graph
          }
        case _ => super.registerAbstraction(graph)(implId , abs)
      }

  def typeEdge(typeUser : NodeId, typeUsed : NodeId) : DGEdge = {
    if(g.getConcreteNode(typeUser).kind.kindType == TypeDecl &&
      typeUser != typeUsed && g.isa_*(typeUser, typeUsed))
      Isa(typeUser, typeUsed)
    else
      Uses(typeUser, typeUsed)
  }

}
