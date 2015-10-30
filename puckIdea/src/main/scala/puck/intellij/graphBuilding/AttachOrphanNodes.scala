package puck.intellij.graphBuilding

import com.intellij.psi.{PsiMember, PsiClass}
import puck.graph.{NodeId, DependencyGraph}
import puck.intellij._

/**
 * Created by lorilan on 23/10/15.
 */
object AttachOrphanNodes {
  def apply
  ( fromId : Int = DependencyGraph.rootId )
  ( implicit builder: IntellijGraphBuilder ) : Unit = {
    import builder.g
    val lastId = g.numNodes - 1
    if(fromId < lastId){
      for(nodeId <- Range.inclusive(fromId, lastId) ){
        //println(s"${g.container(nodeId)} contains $nodeId")
        if(g.container(nodeId).isEmpty && nodeId != g.rootId){
          val n = g.getNode(nodeId)
          //println(s"orphan node : ${n.fullName}  : ${n.kind} - container = ${n.container}")
          builder.graph2ASTMap get nodeId foreach {
            case FieldDeclHolder(field) =>
              addOrphanMemberNode(nodeId, field)
              builder.setFieldType(nodeId, field)
            case MethodDeclHolder(method) =>
              addOrphanMemberNode(nodeId, method)
              builder.setMethodType(nodeId, method)
            //            case Some(ConstructorDeclHolder(cdecl)) => addBodyDecl(cdecl)
            case tdh : TypedKindDeclHolder =>
              addOrphanTypeNode(nodeId, tdh.node)
            case PrimitiveDeclHolder(_)
                 | ArrayTypeWrapper => builder.addContains(builder.getPrimitivePackage, nodeId)
            case sdh =>
              println( g.fullName(nodeId) + " " + sdh + " attach orphan nodes unhandled case")
              ()
          }
        }
      }
      //addBodyDecl (case MethodDeclHolder) can add new typeNodes
      apply(lastId)
    }
  }
  def addOrphanTypeNode
  ( classId : NodeId, psiClass: PsiClass)
  ( implicit builder : IntellijGraphBuilder ) : Unit = {
    val pId = psiClass.getContainingClass match {
      case null =>
        builder.addPackage(QualifiedName.packageName(psiClass), mutable = false)
      case parentClass =>
        GetNode(parentClass)
    }
    builder.addContains(pId, classId)
  }
  def addOrphanMemberNode
  ( memberId : NodeId, member : PsiMember)
  ( implicit builder : IntellijGraphBuilder ) : Unit =
    member.getContainingClass match {
      case null => sys.error(s"IntellijGraphBuilder.addOrphanMemberNode(_, ${member.getName}) no containing class")
      case parentClass =>
        val cId = GetNode(parentClass)
        builder.addContains(cId, memberId)
    }

}
