package puck.graph.comparison

import puck.PuckError
import puck.graph._
import puck.util.Debug


object Mapping {

  def create
  ( g1 : DependencyGraph,
    g2 : DependencyGraph
  ) : Map[NodeId, NodeId] =
    create(nameIndex(g1), nameIndex(g2))

  def nameIndex(g : DependencyGraph) : Map[String, NodeId] = {
    import ShowDG._
    g.nodesId map ( id => ((g, id).shows(sigFullName), id) ) toMap
  }





 def create
  ( m1 : Map[String, NodeId],
    m2 : Map[String, NodeId]
    ) : Map[NodeId, NodeId] = {
   m1.foldLeft(Map[NodeId, NodeId]()){
     case (m, (name, nid1)) =>
       m2 get name match {
         case None => throw new PuckError(s"$name not found while building mapping")
         case Some(nid2) => m + (nid1 -> nid2)
       }
    }
   }
   //  swap(m1).mapValues(m2.apply)

  def swap[A,B]( m : Map[A, B]) : Map[B, A] =
    m.toList.map {case (a,b) => (b,a)}.toMap


  def mapCVM[C[_], V]
  ( mappin : V => V, cvm : CollectionValueMap[V, C, V]) ={
    val l : List[(V, C[V])]= cvm.toList map {
      case (k, v) =>
        (mappin(k), cvm.handler.map(v, mappin))
    }

    new CollectionValueMap(l.toMap, cvm.handler)
  }

  def mapType(mappin : NodeId => NodeId): Type => Type = {
    case NamedType(id) => NamedType(mappin(id))
    case Tuple(lt) => Tuple(lt.map(mapType(mappin)))
    case Arrow(i, o) => Arrow(mapType(mappin)(i), mapType(mappin)(o))
    case ParameterizedType(gid, targs) =>
      ParameterizedType(mappin(gid), targs.map(mapType(mappin)))

    case Covariant(t) => Covariant(mapType(mappin)(t))
    case Contravariant(t) => Contravariant(mapType(mappin)(t))
  }

//  import ShowDG._
//
//  def equalsCVM[C[_], V]
//  ( mappin : V => V)
//  ( cvm1 : CollectionValueMap[V, C, V],
//    cvm2 : CollectionValueMap[V, C, V])
//  (implicit ord: Ordering[V], dgp : (DependencyGraph, DependencyGraph), cb : CordBuilder[V]): Boolean = {
//    val (g1, g2) = dgp
//    def msg(g: DependencyGraph)(k: V, vs: C[V]): String = {
//      s"(${(g, k).shows}, ${cvm1.handler.toList(vs) map (v => (g, v).shows) mkString("[", ",", "]")})"
//    }
//
//    if (cvm1.content.size != cvm2.content.size) {
//      val mappedCvm1 = cvm1.toList map {
//        case (k, vs) => (mappin(k), cvm1.handler.map(vs, mappin))
//      }
//      val diff1 = mappedCvm1 diff cvm2.toList
//      val diff2 = cvm2.toList diff mappedCvm1
//      //error(mkMapStringSortedByKey(cvm1.content) + "<>" + mkMapStringSortedByKey(cvm2.content) +
//      println("diff1 = " + (diff1 map (msg(g1) _).tupled) + "diff2 = " + (diff2 map (msg(g2) _).tupled))
//      false
//    }
//    else
//      cvm1.content.forall {
//        case ((k1, vs1)) =>
//          val vs2 = cvm2.content(mappin(k1))
//          if (cvm1.handler.map(vs1, mappin) != vs2) {
//            error(msg(g1)(k1, vs1) + " " + msg(g2)(mappin(k1), vs2))
//          }
//          true
//
//      }
//  }

  def equalsCVM[C[_], V]
  ( mappin : V => V)
  ( cvm1 : CollectionValueMap[V, C, V],
    cvm2 : CollectionValueMap[V, C, V]) : Boolean =
      cvm1.content.size == cvm2.content.size &&
        cvm1.content.forall {
          case ((k1, vs1)) =>
            val vs2 = cvm2.content(mappin(k1))
            cvm1.handler.map(vs1, mappin) == vs2
        }

    

  def equalsMap[V]
  ( mappin : V => V)
  ( cvm1 : Map[V, V],
    cvm2 : Map[V, V]) : Boolean =
    cvm1.size == cvm2.size &&
      cvm1.forall {
        case ((k1, v1)) =>
          val v2 = cvm2(mappin(k1))
          mappin(v1) == v2
      }


  def equals
  ( g1 : DependencyGraph,
    g2 : DependencyGraph
  ) : Boolean = {
    assert(g1.virtualNodes.isEmpty)
    assert(g2.virtualNodes.isEmpty)

//    implicit val gp = (g1, g2)

    g1.nodesId.size == g2.nodesId.size && {
//    if(g1.nodesId.size != g2.nodesId.size){
//      val fulln1Set = (g1.nodesIndex.concreteNodesId map g1.fullName).toSet
//      val fulln2Set = (g2.nodesIndex.concreteNodesId map g2.fullName).toSet
//      val diff1 = fulln1Set -- fulln2Set
//      val diff2 = fulln2Set -- fulln1Set
//      if(diff1.nonEmpty || diff2.nonEmpty)
//        error("fullName diff1 = " + diff1 + " fullName diff2 = " + diff2)
//      false
//    }
//    else {
      val mappinG1toG2 = create(g1,g2).apply _

      val mappinNodeIdP : NodeIdP => NodeIdP = {
        case (n1, n2) => (mappinG1toG2(n1), mappinG1toG2(n2))
      }

      val equalsNodes = g1.concreteNodes.forall{
        g1n =>
          val g2Id = mappinG1toG2(g1n.id)
          val g2n = g2.getConcreteNode(g2Id).copy(id = g1n.id)
          g1n == g2n
      }

      lazy val equalsUses1 =
        equalsCVM(mappinG1toG2)(g1.edges.userMap, g2.edges.userMap)
      lazy val equalsUses2 : Boolean =
        g1.edges.types.forall {
          case ((id1, t1)) =>
            val t2 = g2.edges.types(mappinG1toG2(id1))
            mapType(mappinG1toG2)(t1) == t2
        }
      lazy val equalsUses3 = g1.edges.accessKindMap.forall{
        case (k, v) =>
          g2.edges.accessKindMap(mappinNodeIdP(k)) == v
      }

      lazy val equalsContains1 =
        equalsCVM(mappinG1toG2)(g1.edges.contents, g2.edges.contents)
      lazy val equalsContains2 =
        equalsCVM(mappinG1toG2)(g1.edges.parameters, g2.edges.parameters)
//      lazy val equalsContains3 =
//        equalsMap(mappinG1toG2)(g1.edges.definition, g2.edges.definition)

      lazy val equalsIsa =
        equalsCVM(mappinG1toG2)(g1.edges.superTypes, g2.edges.superTypes)

      lazy val equalsTD1 =
        equalsCVM(mappinNodeIdP)(g1.edges.typeMemberUses2typeUsesMap,
          g2.edges.typeMemberUses2typeUsesMap)
      lazy val equalsTD2 =
        equalsCVM(mappinNodeIdP)(g1.edges.typeUses2typeMemberUsesMap,
          g2.edges.typeUses2typeMemberUsesMap)

//      println("###############################")
//      println("equalsNodes = " + equalsNodes)
//      println("equalsUses1 = " + equalsUses1)
//      println("equalsUses2 = " + equalsUses2)
//      println("equalsUses3 = " + equalsUses3)
//      println("equalsContains1 = " + equalsContains1)
//      println("equalsContains2 = " + equalsContains2)
//      //println("equalsContains3 = " + equalsContains3)
//      println("equalsIsa = " + equalsIsa)
//      println("equalsTD1 = " + equalsTD1)
//      println("equalsTD2 = " + equalsTD2)

      equalsNodes && equalsUses1 && equalsUses2 && equalsUses3 &&
        equalsContains1 && equalsContains2 && /*equalsContains3 &&*/
        equalsIsa && equalsTD1 && equalsTD2

    }


  }


}
