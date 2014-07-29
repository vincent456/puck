package puck.javaAG

import puck.graph._
import puck.graph.backTrack._
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

class JavaAccessGraph extends AccessGraph[JavaNodeKind](JavaNode){

  Predefined.list.foreach{ p =>
    addNode(p.fullName, p.name, p.kind)
  }

  override def newGraph() : JavaAccessGraph ={
    new JavaAccessGraph()
  }

  def addPackageNode(fullName: String, localName:String) : AGNode[JavaNodeKind] =
    addNode(fullName, localName, puck.javaAG.JavaNodeKind.`package`)

  def addPackage(p : String, mutable : Boolean): AGNode[JavaNodeKind] =
    getNode(p) match {
      case None =>
        val fp = JavaAccessGraph.filterPackageName(p)
        val path = fp split "[.]"
        if (path.length == 0)
          addPackageNode(fp, fp)
        else {
          val (_, n):(StringBuilder, AGNode[JavaNodeKind]) = path.foldLeft(new StringBuilder(), root){
            (sb_nodeParent:(StringBuilder, AGNode[JavaNodeKind]), p:String) => sb_nodeParent match {
              case (sb, nodeParent) =>
                sb append p
                val n = addPackageNode(sb.toString, p)
                n.isMutable = mutable
                nodeParent.content += n
                sb append "."
                (sb, n)
            }
          }
          n
        }
      case  Some(pn) => pn
    }



  def addApiTypeNode(td: AST.TypeDecl, addUses : Boolean): AGNode[JavaNodeKind] = {
    //println("adding api td " + td.fullName())
    val packageNode = addPackage(td.compilationUnit().getPackageDecl, mutable = false)
    val tdNode = addNode(td.fullName(), td.name(), td.getAGNodeKind)
    tdNode.isMutable = false

    if(addUses)
      for (use <- td.uses()) {
        tdNode.users_+=(use.buildAGNode(this))
      }

    //    try{
    packageNode.content += tdNode
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
        tdNode.content += (bd buildAG this)
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
      packageNode.content += strNode
      strNode users_+= bdNode
    }
  }

  private def throwRegisteringError(n : AGNode[JavaNodeKind], astType : String) =
    throw new Error("Wrong registering ! AGNode.kind : %s while AST.Node is an %s".format(n.kind, astType))

  def program = root.kind match {
    case r @ JavaRoot() => r.program
    case _ => throw new Error("root.kind must be JavaRoot")
  }

  def program_=(prog: AST.Program) {
    root.kind match {
      case r @ JavaRoot() => r.program = prog

        Predefined.list.foreach{
          case p @ Predefined(pkg, name, _) =>

            val decl = prog.findType(pkg, name)

            if(decl == null){
              throw new Error(name + " : " + this(p.fullName).kind + ", decl not found")
            }
            (this(p.fullName).kind, decl) match {
              case (p @ Primitive(), d : AST.TypeDecl) => p.decl = d
              case (c @ Class(), d : AST.ClassDecl) => c.decl = d
              case (k, d) => println( k + " with decl of type " +d.getClass + " : case unhandled")
            }
        }
      case _ => assert(false)
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.InterfaceDecl){
    n.kind match {
      case i @ Interface() =>
        i.decl = decl
      case _ => throwRegisteringError(n, "InterfaceDecl")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.ClassDecl){
    n.kind match {
      case c @ Class() => c.decl = decl
      case _ => throwRegisteringError(n, "ClassDecl")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.ConstructorDecl){
    n.kind match {
      case c @ Constructor() => c.decl = decl
      case _ => throwRegisteringError(n, "ConstructorDecl")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.MethodDecl){
    n.kind match {
      case m @ Method() => m.decl = decl
      case m @ AbstractMethod() => m.decl = decl
      case _ => throwRegisteringError(n, "MethodDecl")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.FieldDeclaration){
    n.kind match {
      case f @ Field() => f.decl = decl
      case _ => throwRegisteringError(n, "FieldDeclaration")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.PrimitiveType){
    n.kind match {
      case f @ Primitive() => f.decl = decl
      case _ => throwRegisteringError(n, "PrimitiveType")
    }
  }

  def applyChangeOnProgram(){

    val rec = transformations.recording

    rec.undo()

    /*println( "rec to apply :" )
    rec.foreach(println)*/
    println("applying change !")

    /*val primPkgName = program.typeBoolean().packageName()
    for( c  <-
         scala.collection.JavaConversions.asScalaIterator(program.compilationUnitIterator())){
      val cu: AST.CompilationUnit = c.asInstanceOf[AST.CompilationUnit]

      if(cu.packageName() != primPkgName)
        cu.lockAllNames()
    }*/

    rec.foreach { r =>
      AG2AST(r)
      r.redo()
    }
    program.flushCaches()
    program.eliminateLockedNames()
  }


}
