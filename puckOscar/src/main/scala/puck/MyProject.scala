/**
  * Created by Cedric and Mikal july 2016
  */

import oscar.cp._

import scala.collection.mutable.ArrayBuffer

class Node (n : String, k : Int, e : Option[Boolean] = None) {
  var nom : String = n
  var kind : Int = k
  var external : Boolean = e match {
    case Some(v) => v
    case None => false
  }
}

object MyModel extends CPModel with App {

  // Internal Nodes
  val pnano = 0
  val cClient = 1
  val cPersonne = 2
  val mmain = 3
  val mmainDef = 4
  val mclient = 5
  val mclientDef = 6
  val anom = 7
  val anomDef = 8
  val ccPersonne = 9
  val ccPersonneDef = 10
  val mgetNom = 11
  val mgetNomDef = 12
  val parapmclient = 13
  val paraargsmain = 14
  val paranomPersonne = 15

  val nbInternalNodes = 16

  // Extern Nodes
  val cString = nbInternalNodes +0

  val nbExternalNodes = 1

  val nbNodes = nbInternalNodes + nbExternalNodes

  val kPackage = 1
  val kClass = 2
  val kConstructor = 3
  val kMethod = 4
  val kAttribute = 5
  val kValueDef = 6   // attribute initialisation or method body
  val kparameter = 7

  def printArcs(m: Map[Int,Set[Int]]) = {
    if (m.isEmpty)
      println("No arcs")
    else {
      m.keys.foreach(s =>
        m(s).foreach(t =>
          print("("+s+","+t+") ")))
      println()
    }
  }

  def printArcs2(m : Map[(Int,Int),CPIntVar]) = {
    if (m.isEmpty)
      println("No arcs")
    else {
      m.keys.foreach(x =>
        print(x + " : " + m(x).value + "    "))
    }
    println()
  }

  def arcExists(s:Int, t:Int, m:Map[Int,Set[Int]]) : Int = {
   if(m.contains(s) && m(s).contains(t))
     1
   else 0
  }

  //def getST(s:Int, t:Int, map_orig:Map[Int,Set[Int]]): ()

  val NODES = ArrayBuffer(
    new Node("nano",kPackage), new Node("Client", kClass), new Node("Personne",kClass),
    new Node("main",kMethod), new Node("main", kValueDef),
    new Node("client", kMethod), new Node("client", kValueDef),
    new Node("nom", kAttribute), new Node("nom", kValueDef),
    new Node("Personne", kConstructor), new Node("Personne", kValueDef),
    new Node("getNom", kMethod), new Node("getNom", kValueDef),
    new Node("p", kparameter),
    new Node("args", kparameter),
    new Node("nom", kparameter),
    new Node("String", kClass, Option(true))
  )



  val uses_map_orig =  Map(
    cPersonne -> Set(cPersonne) , mmainDef -> Set(mclient,cPersonne), ccPersonneDef -> Set(anom),
    mgetNomDef ->Set(anom), mclientDef -> Set(mgetNom), mgetNom -> Set(cString)
  )
  println("USES ARCS")
  printArcs(uses_map_orig)


  // var uses_map: Map[CPIntVar, Set[CPIntVar]] = Map()
  // NODES.map(_ => (uses_map += NodeVar -> Set()))

  val contains_map_orig =  Map(
    pnano -> Set(cClient, cPersonne), cClient -> Set(cClient, mmain), cPersonne -> Set(ccPersonne, anom, mgetNom),
    ccPersonne -> Set(paranomPersonne), mclient -> Set(parapmclient), mmain -> Set(paraargsmain))
  println("CONTAINS ARCS")
  printArcs(contains_map_orig)

  val isa_map_orig: Map[Int, Set[Int]] = Map()
  println("ISA ARCS")
  printArcs(isa_map_orig)
  //val isa_map:Map[CPIntVar,Set[CPIntVar]]= Map()

  val type_uses_orig: Map[Int, Set[Int]]= Map(
    anom -> Set(cString), paranomPersonne -> Set(cString), parapmclient -> Set(cPersonne), paraargsmain -> Set(cString)
  )
  println("TYPE USES ARCS")
  printArcs(type_uses_orig)

  var hidden_orig:Map[Int,Set[Int]] = Map()

  //  hide cPersonne and subElements from mclient
  hidden_orig += mclient -> Set(cPersonne, mgetNom, ccPersonne, anom)
  hidden_orig += mclientDef -> Set(cPersonne, mgetNom, ccPersonne, anom)
  hidden_orig += parapmclient -> Set(cPersonne, mgetNom, ccPersonne, anom)
  println("ALL HIDDEN USES ARCS")
  printArcs(hidden_orig)

val qualified_by_orig:Map[(Int, Int),(Int, Int)] = Map (
  (mclientDef, mgetNom) -> ((mclient, cPersonne))
  )

def makeVars(m:Map[Int, Set[Int]]): Map[(Int,Int),CPIntVar] = {
  var result: Map[(Int, Int), CPIntVar] = Map()
  m.keys.foreach(s =>
    m(s).foreach(t =>
      (result += (s, t) -> CPIntVar(0 to 1))
    )
  )
  result
}

  val allUses_orig = uses_map_orig ++ type_uses_orig

  val red_uses = makeVars(allUses_orig)

  allUses_orig.keys.foreach(s =>
    allUses_orig(s).foreach(t =>
      if (arcExists(s,t, hidden_orig) ==1)
        add(red_uses((s, t)) == 1)
      else
        add(red_uses((s, t)) == 0)))

  var nb_red_uses :Int =0
  search {
    binaryFirstFail(red_uses.values.toSeq)
  } onSolution {
    println("The red uses are:")
    printArcs2(red_uses)
//    nb_red_uses = 0
//    red_uses.values.map( x => nb_red_uses+= x.value)
  }


  val maxNewNodes : Int = 2
  var nbNewNode = CPIntVar(0 to maxNewNodes)

  start()

}

// TODO
// deboguer les arcs uses et type uses
// faire varier les arcs uses pour casser les dépendances
// d'abord introduire une interface au dessus de Personne
// comme on a besoin uniquement de la methode getnom on pourrait
// par inférence de type ne definir l'interface qu'avec cette methode

