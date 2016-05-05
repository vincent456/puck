/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package org.extendj.ast

import puck.graph._
import puck.javaGraph.nodeKind.{EnumConstant => PuckEnumConstant, _}
import puck.javaGraph.{JavaGraphBuilder, nodeKind}
import scala.{List => SList}

object JastaddGraphBuilder {

  //JavaAccessor

  def isa(n1 : NodeId, n2 : NodeId) =
      new DGEdge(Isa, n1, n2)

  def classKind : JavaNodeKind = Class
  //def innerClassKind : JavaNodeKind = InnerClass

  def interfaceKind : JavaNodeKind = Interface
  //def innerInterfaceKind : JavaNodeKind= InnerInterface

  def genInterface(gid: GenericInterfaceDecl) : JavaNodeKind = GenericInterface
  def genClass(gid: GenericClassDecl) : JavaNodeKind = GenericClass

  /*{
    Range.inclusive(gid.getTypeParameters.getNumChild - 1, 0, -1 ).foldLeft(List[Variance]()){
      case (l, i) =>
        gid.getTypeParameter(i)
    }

  }*/

  def field = Field
  def staticField = StaticField
  def constructor = Constructor
  def abstractMethod = AbstractMethod
  def method = Method
  def staticMethod = StaticMethod

  def enumConstant = PuckEnumConstant

  def parameter = Param

  def definition = Definition

  def primitive = Primitive
  def typeVariable = nodeKind.TypeVariable
  def wildcardType = WildCardType


  def definitionName = DependencyGraph.definitionName


  def qualifierIsThisAccess(access : TypeMemberAccess) : Boolean =
    (!access.isQualified) &&
      access.decl().hostType() == access.hostType() ||
      access.isQualified &&
      access.qualifier.isThisAccess


  def qualifierIsSuperAccess(access : TypeMemberAccess) : Boolean =
    (!access.isQualified) &&
      access.decl().hostType() != access.hostType() ||
      access.isQualified &&
      access.qualifier.isSuperAccess


}

import JastaddGraphBuilder.{qualifierIsSuperAccess, qualifierIsThisAccess}

class JastaddGraphBuilder(val program : Program)
  extends JavaGraphBuilder
  with TypeUsage
  with GraphBuilderVisitor
  with Registration {
  var graph2ASTMap = Map[Int, ASTNodeLink]()

  def findTypeDecl(typ : String): TypeDecl ={
    val td = program findType typ
    if(td == null)
      throw new DGBuildingError(typ + " not found")
    td
  }

  def getNode(n : DGNamedElement): NodeId =
    super.addNode(n.dgFullName(), n.name(), n.getDGNodeKind, n.fromSource){
      nid => n.registerNode(this, nid)
    }

  import JastaddGraphBuilder.definitionName
  def getDefNode(n : DGNamedElement): NodeId =
    super.addNode(n.dgFullName()+ "." + definitionName, definitionName, Definition, n.fromSource)()

  def attachOrphanNodes(fromId : Int = g.rootId) : Unit = {
    val lastId = g.numNodes - 1
    if(fromId < lastId){
      for(nodeId <- Range.inclusive(fromId, lastId) ){
        //println(s"${g.container(nodeId)} contains $nodeId")
        if(g.container(nodeId).isEmpty && nodeId != g.rootId){
          val n = g.getNode(nodeId)
          //println(s"orphan node : ${g.fullName(nodeId)}  : ${n.kind}")
          graph2ASTMap get nodeId match {
            case Some(FieldDeclHolder(d, _)) => addBodyDecl(d)
            case Some(MethodDeclHolder(d)) => addBodyDecl(d)
            case Some(EnumConstantHolder(d)) => addBodyDecl(d)
            case Some(ConstructorDeclHolder(cdecl)) =>
              nodesByName get cdecl.hostType().dgFullName() match {
                case Some(pid) => cdecl.buildDG(this, pid)
                case _ => addBodyDecl(cdecl)
              }
            case Some(tdh : TypedKindDeclHolder) => addApiTypeNode(tdh.decl)
            case sdh =>
              println( g.fullName(nodeId) + " " + sdh + " attach orphan nodes unhandled case")
              ()
          }
        }
      }
      //addBodyDecl (case MethodDeclHolder) can add new typeNodes
      attachOrphanNodes(lastId)
    }
  }

  import scala.collection.JavaConversions._
  def addParams(decl : NodeId, params : java.util.ArrayList[Integer]) : Unit =
    addParams(decl, params.toList.map(_.toInt))

  def buildDGType(thisId : NodeId, decl : FieldDeclarator) : Unit = {
    setType(thisId,  getType(decl.getTypeAccess))
  }

  def buildDGType(thisId : NodeId, decl : BodyDecl) : Unit = decl match {
    case cDecl : ConstructorDecl =>
      astParamsToDGType(thisId, cDecl)
      setType(thisId, NamedType(cDecl.hostType() buildDGNode this))
    case mDecl : MethodDecl =>
      astParamsToDGType(thisId, mDecl)
      setType(thisId, getType(mDecl.getTypeAccess))
    case _ : SubstitutedBodyDecl =>
      println("SubstitutedBodyDecl.buildDGType TODO !!!")
    case _ : FieldDecl =>
      throw new DGBuildingError("missing type for FieldDecl")
    case _ : EnumConstant =>
      throw new DGBuildingError("missing type for EnumConstant")
    case _ => ()

  }



  def astParamsToDGType(thisId : NodeId, c : Callable ) : Unit = {
    addParams(thisId,
      c.getParameterList.foldRight(SList[NodeId]()) {
      case (pdecl, params) =>
        val paramId = pdecl buildDGNode this
        setType(paramId, getType(pdecl.`type`()))
        paramId :: params
    })
  }


  def addApiNode(nodeKind : String, typ : String, bodydeclName: String) : Unit = {
    //println("trying to add type "+typ + " " + bodydeclName+ " ... ")

    val td = findTypeDecl(typ)
    val typeNodeId = addApiTypeNode(td)

    def addBodyDecl(bd : BodyDecl) : Unit = {
      if(bd == null)
        System.err.println("Method or constructor" + bodydeclName + " not found in the program ...")
      else
        bd.buildDG(this, typeNodeId)
    }

    nodeKind match {
      case "type" => ()
      case "method" => addBodyDecl (td findMethodBySig bodydeclName)
      case "constructor" => addBodyDecl (td findConstructorBySignature ("#_" + bodydeclName))

      case _ => System.err.println("node kind unknown")
    }
  }

  def buildTypeUse(tmAccess : TypeMemberAccess, typeMemberUse: Uses) : Unit = {
    if(qualifierIsThisAccess(tmAccess)) {
      val thisTypeId = tmAccess.hostType().buildDGNode(this)
      addEdge(addBinding(thisTypeId, thisTypeId, typeMemberUse))
    }
    else if(qualifierIsSuperAccess(tmAccess)){
      val thisTypeId = tmAccess.hostType().buildDGNode(this)
      val superTypeId = tmAccess.hostType.asInstanceOf[ClassDecl].superclass().buildDGNode(this)
      addEdge(addBinding(thisTypeId, superTypeId, typeMemberUse))
    }
    else {
      val access = tmAccess.asInstanceOf[Access]
      try findTypeUserAndBindUses(typeMemberUse, access.qualifier())
      catch {
        case _: Error =>
            throw new NoTypeUser(access.prettyPrint() + "(" + access.getClass + ") in " +
              access.compilationUnit().pathName() + " " + access.location())


      }
    }
  }

  //a.b.c()
  //typeMemberUse, used is c
  //b is qualifier
  //a is qualifier's qualifier


  def addApiTypeNode(td: TypeDecl): NodeIdT = {
    val tdNode = getNode(td)

    val cterId =
      if(td.isTopLevelType)
        addPackage(td.packageName(), mutable = false)
      else
        td.getParentNamedNode.buildDGNode(this)


    addContains(cterId, tdNode)
    tdNode
  }

  def addApiTypeNodeAndRegister(td: TypeDecl): Unit =
    registerDecl(addApiTypeNode(td), td)


  def addBodyDecl(bd : BodyDecl) : Unit = {
    val typeNodeId = addApiTypeNode(bd.hostType())
    bd.buildDG(this, typeNodeId)
  }

  def getParamType(parTypeDecl : ParTypeDecl) : Type = {
    val genId = getNode(parTypeDecl.genericDecl())
    val args =
      Range.inclusive(parTypeDecl.numTypeParameter() - 1, 0, -1).foldLeft(scala.List[Type]()) {
        case (l, i) =>
          getType(parTypeDecl.getParameterization.getArg(i)) :: l
      }
    ParameterizedType(genId, args)
  }

  def getType(td : TypeDecl) : Type = td match {
    case parTypeDecl : ParTypeDecl => getParamType(parTypeDecl)
    case wst : WildcardSuperType => Contravariant(getType(wst.getAccess))
    case wet : WildcardExtendsType => Covariant(getType(wet.getAccess))
    case _ => NamedType(getNode(td))
  }



  def getType(a : Access) : Type = {
    a.lock()
    a match {
      case aa: ArrayTypeAccess =>
        ParameterizedType(arrayTypeId, scala.List(getType(aa.getAccess)))
      case bta : BoundTypeAccess => getType(bta.getTypeDecl)
      case ta: TypeAccess => NamedType(getNode(ta.decl()))
      case d: Dot =>
        if (d.isRightRotated)
          d.rotateLeft()
        getType(d.getRight)
      case pta: ParTypeAccess => getType(pta.`type`())
      case we: AbstractWildcard => getType(we.`type`())
      case _ => throw new Error(s"getType, ${a.compilationUnit().pathName()} line ${a.location()} " +
        s"access.getClass == ${a.getClass}")
    }
  }


  def addPrimitiveTypes( p : Program) : Unit = {
    val pcu = p.getPrimitiveCompilationUnit
    SList(pcu.typeBoolean(), pcu.typeByte(), pcu.typeShort(), pcu.typeChar(),
      pcu.typeInt(), pcu.typeLong(), pcu.typeFloat(), pcu.typeDouble(),
      pcu.typeVoid(), pcu.typeNull()) foreach addApiTypeNodeAndRegister
  }


}
