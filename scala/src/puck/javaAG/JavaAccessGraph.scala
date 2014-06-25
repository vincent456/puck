package puck.javaAG

import puck.graph._
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.mutable.StringBuilder

/**
 * Created by lorilan on 07/05/14.
 */

object JavaAccessGraph {

  val defaultPackageName = "<default package>"

  def filterPackageName(name: String) = name match {
    case "" => defaultPackageName
    case _ => name
  }
}

class JavaAccessGraph extends AccessGraph(JavaNode){
  List(Primitive.voidNode(this),
    Primitive.booleanNode(this),
    Primitive.byteNode(this),
    Primitive.charNode(this),
    Primitive.doubleNode(this),
    Primitive.floatNode(this),
    Primitive.intNode(this),
    Primitive.longNode(this),
    Primitive.shortNode(this),
    Primitive.stringNode(this)) foreach {
    (n : AGNode) =>
      nodesByName += (n.name -> n)
      addNode(n)
  }

  def addPackageNode(fullName: String, localName:String) : AGNode =
    addNode(fullName, localName, puck.javaAG.JavaNodeKind.`package`)

  def addPackage(p : String, mutable : Boolean): AGNode =
    getNode(p) match {
    case None =>
      val fp = JavaAccessGraph.filterPackageName(p)
      val path = fp split "[.]"
      if (path.length == 0)
        addPackageNode(fp, fp)
      else {
        val (_, n):(StringBuilder, AGNode) = path.foldLeft(new StringBuilder(), root){
          (sb_nodeParent:(StringBuilder, AGNode), p:String) => sb_nodeParent match {
            case (sb, nodeParent) =>
              sb append p
              val n = addPackageNode(sb.toString, p)
              n.isMutable = mutable
              nodeParent content_+= n
              sb append "."
              (sb, n)
          }
        }
        n
      }
     case  Some(pn) => pn
  }



  def addApiTypeNode(td: AST.TypeDecl, addUses : Boolean): AGNode = {
    //println("adding api td " + td.fullName())
    val packageNode = addPackage(td.compilationUnit().getPackageDecl, mutable = false)
    val tdNode = addNode(td.fullName(), td.name(), td.getAGNodeKind)
    tdNode.isMutable = false

    if(addUses)
      for (use <- td.uses()) {
        tdNode.users_+=(use.buildAGNode(this))
      }

//    try{
      packageNode content_+= tdNode
//    }catch{
//      case e : IllegalAGOperation => ()
//    }

    tdNode
  }


  def addApiNode(program : AST.Program, nodeKind : String, `type` : String, bodydeclName: String){
    println("trying to add type "+`type` + " " + bodydeclName+ " ... ")
    val td = program  findType `type`
    if(td == null){
      System.err.println(`type` + " not found")
      return
    }

    val tdNode = addApiTypeNode(td, addUses = true)

    def addBodyDecl(bd : AST.BodyDecl){
      if(bd == null)
        System.err.println("Method or constructor" + bodydeclName + " not found in the program ...")
      else
        tdNode content_+= (bd buildAG this)
    }

    nodeKind match {
      case "type" => ()
      case "method" => addBodyDecl (td findMethodBySig bodydeclName)
      case "constructor" => addBodyDecl (td findConstructorBySignature ("#_" + bodydeclName))

      case _ => System.err.println("node kind unknown")
    }
  }

  def addStringLiteral(literal: String, occurrences: _root_.java.util.Collection[AST.BodyDecl]){
    println("string "+literal + " "+ occurrences.size()+" occurences" )

    for(bd <- occurrences){
      val packageNode = this(bd.hostBodyDecl.compilationUnit.getPackageDecl)

      val bdNode = bd buildAGNode this
      val strNode = addNode(bd.fullName()+literal, literal,
        puck.javaAG.JavaNodeKind.literal(new JavaType(this("java.lang.String"))))

      /*
        this is obviously wrong: TODO FIX
      */
      packageNode content_+= strNode
      strNode users_+= bdNode
    }
  }

}
