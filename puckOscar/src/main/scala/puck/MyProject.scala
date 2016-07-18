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
  override def toString = {
    "(" + nom + ", " +kind + ", " + external +")"
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
  val kInterface = 8

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

  def printQualified( m: Map[(Int, Int),(Int, Int)]) = {
    if (m.isEmpty)
      println("No arcs")
    else {
      m.keys.foreach(x =>
        print(x + " : " + m(x) + "    "))
    }
    println()
  }

  def printNodes(nodes : ArrayBuffer[Node]): Unit = {
    nodes.map(x => print("" + x +", "))
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
  println("NODES")
  printNodes(NODES)

  val abstracted_nodes : Array[CPIntVar] = Array(
    CPIntVar(-1 to NODES.size-1)
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
  (mclientDef, mgetNom) -> ((parapmclient, cPersonne)),
  (parapmclient, cPersonne) -> ((parapmclient, cPersonne))
  )
  println("QUALIFIED_BY ARCS")
  printQualified(qualified_by_orig)

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


  // pour tout noeud abstracted
  // sa valeur doit correspondre à un dominant
  // d'un red uses
  // sinon on met -1

  // Reformulation
  // pour pour tout noeud abstracted
  // soit sa valeur est -1
  // soit c'est l'abstraction d'un noeud qui est à droite d'un arc
  // qui domine

  var y : CPIntVar = CPIntVar(0 to nbNodes)
  println("$$$ "+y)
 // abstracted_nodes.foreach(x =>
 //    println("$$$$$ " +(red_uses.toArray)(0))
 // )

  var nb_red_uses :Int =0
  search {
    binaryFirstFail(red_uses.values.toSeq ++ abstracted_nodes.toSeq ++ Seq(y))
  } onSolution {
    println("The red uses are:")
    printArcs2(red_uses)
    println("abstracted nodes:")
    abstracted_nodes.map(x => print("" + x))
    println()

    nb_red_uses = 0
    red_uses.values.map( x => nb_red_uses+= x.value)
    println("nb violations  = " + nb_red_uses)
    println("reduses to Array")
    println(red_uses.toArray.mkString(","))


 //   if (nbNewNode.value ==1)
 //     NODES += new Node("Personne_ABS", kInterface)
 //   println("NODES")
 //   printNodes(NODES)
  }

  val maxNewNodes : Int = 1 // because 1 qualifies all the violations
  var nbNewNode = CPIntVar(0 to maxNewNodes)

  start()
}

// TODO
// faire varier les arcs uses pour casser les dépendances
// d'abord introduire une interface au dessus de Personne
// comme on a besoin uniquement de la methode getnom on pourrait
// par inférence de type ne definir l'interface qu'avec cette methode

// on ajoute donc un noeud SI cela permet de diminuer les violations
// donc on veut A TERME optimiser NBViolations + NBNewNodes

// MAIS dans un premier temps on va essayer de simplement ajouter un noeud : une interface
// Etape 1 : incrementer maxNewNodes  et on crée le noeud

