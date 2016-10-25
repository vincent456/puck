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
import scala.{List => SList}
import scala.collection.JavaConversions._
/**
  * Created by Loïc Girault on 18/04/16.
  */
trait GraphBuilderVisitor {
  this: JastaddGraphBuilder =>

  def buildDG(containerId: NodeId, td: TypeDecl): Unit = td match {
    case _: WildcardExtendsType | _: WildcardSuperType => ()
    case _ =>
      val n = buildTypeDecl(td)
      addEdge(Contains(containerId, n))
      if (td.isParameterizedType) {
        val ptd = td.asInstanceOf[ParTypeDecl]
        println("[BUILD] parameterized type " + ptd.nameWithArgs())
      }
      td.getBodyDeclList foreach (_.buildDG(this, n))
  }

  def buildTypeVariables(tid: NodeId, gtd: GenericElement): Unit =
    for (i <- (gtd.getNumTypeParameter -1).to(0, -1) ){ //reverse order as params are ordered and prepended
        val tv = gtd getTypeParameter i
        addEdge(ContainsParam(tid, this buildNode tv))
    }

  def buildInheritence(tdecl: TypeDecl) : Type = {
    val sub = getType(tdecl)

    //either super classes are in code and they will be visited and the type hierarchy completed
    //either they are in libraries and type hierarchy will be completed upon "attach orphan nodes" pahe
    def add(supAccess: Access) : Unit = addIsa(sub, getType(supAccess))

    tdecl match {
      case cd : ClassDecl =>
        // addExtends
        if(cd.hasSuperclass && cd.superclass().fullName() != "java.lang.Object")
          add(cd.getSuperClass)

        cd.getImplementss foreach add
      case id : InterfaceDecl =>
        id.getSuperInterfaces foreach add

      case _ => ()
    }
    sub
  }

  def buildTypeDecl(td : TypeDecl) : NodeId = td match {
    case tv : TypeVariable => this buildNode tv
    case cd : ClassDecl =>

      val n = this buildNode cd

      buildInheritence(cd)
      if(cd.isGenericType)
        buildTypeVariables(n, cd.asInstanceOf[GenericElement])

      if(cd.hasImplicitConstructor)
        cd.getImplicitConstructor.buildDG(this, n)

      n
    case id : InterfaceDecl =>
      val n = this buildNode id
      buildInheritence(id)
      if(id.isGenericType)
        buildTypeVariables(n, id.asInstanceOf[GenericElement])

      n

    case _ =>
      throw new DGBuildingError(s"ClassDecl or InterfaceDecl exptected, $td found");
  }


  def buildDG(containerId : NodeId, constructorDecl: ConstructorDecl) : Unit = {
    resetLocal()
    val declId = buildNode(constructorDecl)
    addEdge(Contains(containerId, declId))
    buildDGType(declId, constructorDecl)

    //buildMethodOrConstructorDG(containerId, declId, constructorDecl)
    constructorDecl.getExceptionList.buildDG(this, declId)
    constructorDecl.buildDef(this, declId)
    constructorDecl match {
      case genConstructorDecl : GenericConstructorDecl =>
        genConstructorDecl.getTypeParameterList.buildDG(this, declId)
      case _ => ()
    }
  }

  def buildDG(containerId : NodeId, methodDecl : MethodDecl) : Unit ={
    resetLocal()
    val declId = buildNode(methodDecl)
    addEdge(Contains(containerId, declId))

    buildDGType(declId, methodDecl)

    methodDecl match {
      case pmd : ParMethodDecl => buildTypeVariables(declId, pmd.genericMethodDecl())
      case gmd : GenericMethodDecl => buildTypeVariables(declId, gmd)
      case _ => ()
    }

    methodDecl.getExceptions.foreach {
      exc =>
        addUses(declId, buildNode(exc))
    }

    //buildMethodOrConstructorDG(containerId, declId, methodDecl)

    //methodDecl.getTypeAccess.buildDG(this, declId) //return type
    //methodDecl.getParameterList.buildDG(this, declId)
    methodDecl.getExceptionList.buildDG(this, declId)
    if(methodDecl.hasBlock) {
      puck.ignore(methodDecl.buildDef(this, declId))
    }
  }

  def buildMethodOrConstructorDG(containerId : NodeId, declId : NodeId, node : BodyDecl) : Unit = {
    if(node.hasDefinition){
      for( i <- 0 until node.getNumChild){

        if( i != node.getDefIndex
          && i != node.getParamIndex
          && i != node.getReturnTypeIndex ) {
          println("buildMethodOrConstructorDG building " + node.getChild(i))
          node.getChild(i).buildDG(this, declId)
        }
      }
      val defId = node.asInstanceOf[DGNamedElement].buildDef(this, declId)
    }
    else {
      for (i <- 0 until node.getNumChild) {
        if (i != node.getParamIndex
          && i != node.getReturnTypeIndex) {
          println("buildMethodOrConstructorDG building " + node.getChild(i))
          node.getChild(i).buildDG(this, declId)
        }
      }
    }
  }

  def buildDG(containerId : NodeId, pta : ParTypeAccess) : Unit = {
    getType(pta).ids.foreach(id => addEdge(Uses(containerId, id)))
    pta.buildDGInChildren(this, containerId)
  }

  def buildDG(containerId : NodeId, expr : ClassInstanceExpr) : Unit = {
    //    def onAccess(access : Access ) : Unit =
    //      access match {
    //        case _ :TypeAccess => ()
    //        case pta : ParTypeAccess =>
    //          pta.getTypeArguments.foreach {
    //            ta => ta.buildDG(this, containerId)
    //          }
    //        case d : AbstractDot => onAccess(d.getRight)
    //        case da : DiamondAccess => onAccess(da.getTypeAccess)
    //        case _ => throw new DGBuildingError(s"ClassInstanceExpr access kind ${access.getClass} not handled " +
    //          s"in ${access.compilationUnit().pathName()} line ${access.location()}")
    //      }
    //
    //    onAccess(expr.getAccess)

    expr.getArgList.buildDG(this, containerId)
    expr.getTypeDeclOpt.buildDG(this, containerId)


    val ctorNodeId = this buildNode expr //does lock
    if( expr.hasTypeDecl )
      expr setTarget null//unlock anonymous decl

    addEdge(Uses(containerId, ctorNodeId))
  }

  def buildDG(containerId : NodeId, stmt : EnhancedForStmt) : Unit = {
    stmt.getExpr.buildDGInChildren(this, containerId)
    stmt.getStmt.buildDGInChildren(this, containerId)
    stmt.getExpr match {
      case access : Access =>
        val leftval = this buildNode stmt.getVariableDecl
        addContains(containerId, leftval)
        setType(leftval, getType(stmt.getTypeAccess))


        val tcBuilder : NodeId => TypeConstraint =
          stmt.getExpr.`type`() match {
          case typUsed : ArrayDecl =>

            val arg = NamedType(buildNode(typUsed.elementType()))
            val iterableParType = ParameterizedType(arrayTypeId, SList(arg))
            id =>
              AndTypeConstraint( SList(Sub(TypeOf(leftval),
                ParTypeProjection(TypeVar(iterableParType), 0)),
                Sub(TypeOf(id), TypeVar(iterableParType))))

          case t : TypeDecl =>
            val iterable : ParInterfaceDecl =
              if(t.fullName() startsWith "java.lang.Iterable") t.asInstanceOf[ParInterfaceDecl]
              else
                t.supertypestransitive().find(_.name == "Iterable") match {
                  case Some(iter) => iter.asInstanceOf[ParInterfaceDecl]
                  case None =>
                    puck.error(s"Expr in ${stmt.prettyPrint()}, typed ${t.fullName()} should be a subtype of Iterable (${stmt.fullLocation()})")
                }

            val arg = NamedType(buildNode(iterable.getParameterization.getArg(0)))
            val iterableParType = ParameterizedType(buildNode(iterable), SList(arg))

            id =>
              AndTypeConstraint( SList(Sub(TypeOf(leftval),
                ParTypeProjection(TypeVar(iterableParType), 0)),
                Sub(TypeOf(id), TypeVar(iterableParType))))

        }

        getQualifiers(access.lastAccess()) foreach {
          rightAccess =>
            addTypeConstraint(tcBuilder(this buildNode rightAccess))
        }
      case _ => println("type constraint in : " + stmt.prettyPrint() + " not handled")
    }
  }
  def buildDG(containerId : NodeId, stmt : VarDeclStmt) : Unit = {
    val t = getType(stmt.getTypeAccess)
    //t.ids.foreach (id => addEdge(Uses(containerId, id)))

    val astType = stmt.`type`()

    stmt.getDeclarators map {
      vd =>
        val vdId = this buildNode vd
        addEdge(Contains(containerId, vdId))
        setType(vdId, t)
        (vd, vdId)
    } filter(_._1.hasInit) foreach {
      case (vd, vdId) =>
        vd.getInit.buildDG(this, containerId)
        vd.getInit match {
          case a: Access => constraintTypeUses(TypeOf(vdId), astType, a)
          case _ => ()
        }
    }
  }

  def buildDG(containerId : NodeId, expr : AssignExpr) : Unit = {
    expr.getSource.buildDG(this, containerId)
    expr.getDest.buildDG(this, containerId)

    val astType = expr.getDest.`type`()
    val destNode = expr.getDest match {
      case a : Access => buildNode(a.lastAccess())
      case _ => throw new DGBuildingError()
    }

    expr.getSource match {
      case src : Access =>
        constraintTypeUses(TypeOf(destNode), astType, src.lastAccess())
      case _ => ()
    }
  }

  def registerGetterSetter(fid : NodeId, fieldDeclarator: FieldDeclarator) : Unit = {
    val smallName = fieldDeclarator.name().toLowerCase
    val setterName = "set" + smallName
    val getterName =
      (if(fieldDeclarator.`type`() == program.typeBoolean()) "is"
      else "get") + smallName

    var getter : Option[NodeId] = None
    var setter : Option[NodeId] = None

    fieldDeclarator.hostType().methodsNameMap() foreach {
      case (name, decls) =>
        if(name.toLowerCase == setterName && decls.size() == 1){
          setter = Some(getNode(decls.iterator().next()))
        }
        if(name.toLowerCase == getterName && decls.size() == 1){
          getter = Some(getNode(decls.iterator().next()))
        }
    }

    if(getter.nonEmpty || setter.nonEmpty)
      g = g.addAbstraction(fid, ReadWriteAbstraction(getter, setter))
  }

  def buildDG(containerId : NodeId, fieldDecl : FieldDecl) : Unit = {
    val t = getType(fieldDecl.getTypeAccess)

    val astType = fieldDecl.`type`()

    fieldDecl.getDeclarators.foreach {
      fd =>
        val fdId = this buildNode fd
        addEdge(Contains(containerId, fdId))
        setType(fdId, t)

        registerGetterSetter(fdId, fd)

        if( fd.hasInit ) {
          puck.ignore(buildFieldInit(fdId, fd, fd.getInit))
          fd.getInit match {
            case a: Access => constraintTypeUses(TypeOf(fdId), astType, a)
            case _ => ()
          }
        }
    }
  }


  def buildFieldInit(fieldId : NodeId, field : FieldDeclarator, init : Expr) : NodeId ={
    val defId = getDefNode(field)
    registerDef(defId, init)
    addEdge(Contains(fieldId, defId))
    init.buildDG(this, defId)
    defId
  }

  def buildDef(defOwner : DGNamedElement,
               theDef : Block , defOwnerId: NodeId) : NodeId = {
    val defId = getDefNode(defOwner)
    theDef.registerDef(this, defId)
    addEdge(Contains(defOwnerId, defId))
    theDef.buildDGInChildren(this, defId)
    defId
  }

  def buildDG(containerId : NodeId, pd : ParameterDeclaration) : Unit = {
    val pid = buildNode(pd)
    addEdge(Contains(containerId, pid))
    val t = getType(pd.getTypeAccess)
    setType(pid, t)
  }

  def buildDG(containerId : NodeId, va : ConstructorAccess) : Unit = {
    addEdge(Uses(containerId, buildNode(va)))
    va.buildDGInChildren(this, containerId)
  }
  def buildDG(containerId : NodeId, va : VarAccess) : Unit =
    if(va. decl().isField){
      val nodeId = this buildNode va
      val typeMemberUses = Uses(containerId, nodeId)
      addEdge(typeMemberUses)

      if(!va.isDeclStatic)
        buildTypeUse(va, typeMemberUses, va.usesAccessKind())
    }

  def buildDG(containerId : NodeId, ta : TypeAccess) : Unit =
    addEdge(Uses(containerId, this buildNode ta))

  def buildDG(containerId : NodeId, ma : MethodAccess) : Unit = if(ma.fromSource()){
    if(!(ma.decl().hostType().isEnumDecl  && ma.decl().location() == "0"))
      ma.lock()

    val decls = ma.decls_keepMethodsInDifferentTypeHierarchy()
    if(!decls.isSingleton()){
      println(s"Warning ! method access ${ma.name()}" +
        s" in ${ma.compilationUnit().pathName()} line ${ma.location()}" +
        " refers to several declaration : ")
      decls.foreach(d => println(d.dgFullName()))
    }
    ma.getArgs foreach ( _.buildDG(this, containerId) )


    //99% du temps il y'a une seule déclaration mais dans des cas ou une classe implémente
    //plusieurs interfaces indépendante possédant une signature commune, on peux avoir plusieurs déclaration
    decls foreach {
      decl =>
        val nodeId = this buildNode decl.asInstanceOf[DGNamedElement]
        val typeMemberUses = Uses(containerId, nodeId)
        addEdge(typeMemberUses)

        if(!decl.isStatic)
          buildTypeUse(ma, typeMemberUses, None)

        if(! decl.isSubstitute)
          decl.getParameterList.toList.zip(ma.getArgs.toList) foreach putConstraintOnArg
        else {
          val substitutedDecl = decl.asInstanceOf[MethodDeclSubstituted]
          val genDecl = substitutedDecl.sourceMethodDecl()

          genDecl.getParameterList.toList.zip(
            substitutedDecl.getParameterList.toList).zip(
            ma.getArgs.toList) foreach putConstraintOnSubstitutedArg(ma)

        }
    }
  }




  def buildDG(containerId : NodeId, rs : ReturnStmt) : Unit = {
    rs.buildDGInChildren(this, containerId)
    if(rs.fromSource()) normalizeAccessAndApply (
      {a =>
        val methodNode = this buildNode rs.hostBodyDecl()
        val astType = rs.hostBodyDecl().asInstanceOf[MethodDecl].`type`()
        constraintTypeUses(TypeOf(methodNode), astType, a.lastAccess())},
      { case null =>()
      // empty return. Since the code compiles, it is well typed and
      // it means the return type is void and no constraint is needed
      case e => throw new DGBuildingError(s"buildDG ReturnStmt case not expected : $e " +
        rs.compilationUnit().pathName() + " line " + rs.location())
      }

    )(rs.getResult)
  }

}
