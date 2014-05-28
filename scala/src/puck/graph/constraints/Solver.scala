package puck.graph.constraints

import puck.graph._
import scala.Some

/**
 * Created by lorilan on 28/05/14.
 */
trait Solver {

   val graph : AccessGraph

   val violationsKindPriority : List[NodeKind]

   def priorityViolationTarget : Option[AGNode] = {

    def aux (priorities : List[NodeKind]) : Option[AGNode] = priorities match {
      case topPriority :: tl => graph.iterator.find{ (n : AGNode) =>
        n.kind == topPriority && ! n.wrongUsers.isEmpty
      }
      case List() => graph.iterator.find{ ! _.wrongUsers.isEmpty }
    }

    aux(violationsKindPriority)
  }

   def redirectTowardExistingAbstractions(usee: AGNode, wrongUsers : List[AGNode]) = {
     wrongUsers.foldLeft(List[AGNode]()){(unsolved : List[AGNode], wu : AGNode) =>
        usee.abstractions find {!wu.interloperOf(_)} match {
          case None => wu :: unsolved
          case Some( abs ) => wu redirectUses (usee, abs)
          unsolved
        }
     }
   }

   def findHost(impl: AGNode, absKind : NodeKind,
                wrongUsers: List[AGNode]) : Option[AGNode] = {

     def find(predicate : (AGNode) => Boolean)  = {
       graph.iterator.find { (n: AGNode) =>
         n.canContain(absKind) &&
           predicate(n) &&
           wrongUsers.forall(!_.interloperOf(n))
       }
     }

     if(impl.isUserOfItsAbstractionKind)
       find{ (absHost : AGNode) => !(impl interloperOf absHost) }
     else
       find{ (absHost : AGNode) => !(absHost interloperOf impl) }

   }

   def singleAbsIntro (impl : AGNode, wrongUsers : List[AGNode]) : Boolean ={
     // TODO find a strategy or way to make the user choose which abstractkind is used !
     findHost(impl, impl.kind.abstractKinds.head, wrongUsers) match {
        case None => false
        case Some(host) =>
          val abs = impl.createAbstraction()
          host.content_+=(abs)
          wrongUsers.foreach(_.redirectUses(impl, abs))
         true
     }
   }
   def multipleAbsIntro (impl : AGNode, wrongUsers : List[AGNode]) : Boolean ={
    throw new AGError("multipleAbsIntro not implemented")
   }
   def hostIntro (impl : AGNode, wrongUsers : List[AGNode]) : Boolean = {
     throw new AGError("hostIntro not implemented")
   }

   def intro(impl : AGNode, wrongUsers : List[AGNode]) {
       if(!singleAbsIntro(impl, wrongUsers)){
         if(!multipleAbsIntro(impl,wrongUsers))
           hostIntro(impl, wrongUsers)
       }
   }

   def solve(){

     priorityViolationTarget match {
          case None => ()
          case Some(target) =>
            target.searchExistingAbstractions()
            val unsolved = redirectTowardExistingAbstractions(target, target.wrongUsers)
            solve()
     }
   }
}
