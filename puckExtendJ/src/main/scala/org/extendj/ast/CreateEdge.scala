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
import org.extendj.concretize.CreateNode
import puck.javaGraph._
import puck.javaGraph.nodeKind._
import puck.util.PuckLogger



object CreateEdge {

  def createTypeAccess
  (id2declMap: NodeId => ASTNodeLink,
   typ : Type)
  ( implicit program : Program): Access = (typ, id2declMap(Type.mainId(typ)) ) match {
    case (NamedType(_), TypeVariableHolder(tvdecl)) => tvdecl.createLockedAccess()
    case (NamedType(_), TypedKindDeclHolder(tdecl)) => tdecl.createLockedAccess()
    case (ParameterizedType(_, args), TypedKindDeclHolder(gt : GenericTypeDecl)) =>
      val argsAccesses = args.map(createTypeAccess(id2declMap, _))

      val pta = ParTypeAccess.create(gt.createLockedAccess(), argsAccesses)
      pta.setParent(program)
      val ptd = gt.lookupParTypeDecl(pta)
      ptd.createLockedAccess()
    case (t, dh) => throw new JavaAGError(s"CreateEdge.createTypeUse: ($t, $dh) where expected a typeDecl")
  }

  def createTypeUse
  (resultGraph : DependencyGraph,
   id2declMap: NodeId => ASTNodeLink,
   typed : NodeId,
   typ : Type)
  ( implicit program : Program): Unit =
    if(resultGraph.getConcreteNode(typed).kind != LocalVariable) // local variable creation happens when redirecting use of initiailizaion in factory
      id2declMap(typed)  match {
        //explicit upcast shouldn't be needed, why the compiling error ?
        case dh @ (FieldDeclHolder(_,_)
                   | ParameterDeclHolder(_)
                   | MethodDeclHolder(_)
                   | LocalVarDeclHolder(_) ) =>
          dh.asInstanceOf[HasNode].node.setTypeAccess(createTypeAccess(id2declMap, typ))

        case ConstructorDeclHolder(_) => ()

        case k => throw new JavaAGError(s"CreateEdge.createTypeUse: $k as user of TypeKind, set type unhandled !")
      }


  def apply
  ( e: DGEdge)
  ( implicit program : Program,
    logger : PuckLogger,
    resultAndReenactor : (DependencyGraph, DependencyGraph),
    id2declMap: NodeId => ASTNodeLink ) = {
    val (resultGraph, reenactor) = resultAndReenactor
    e.kind match {
      case Contains =>
        createContains(resultGraph, reenactor, id2declMap, e)
      case ContainsParam =>
        (id2declMap(e.container), id2declMap(e.content)) match {
          case (MethodDeclHolder(mdecl), ParameterDeclHolder(pdecl)) =>
            mdecl.prependParameter(pdecl)
          case (ConstructorDeclHolder(cdecl), ParameterDeclHolder(pdecl)) =>
            cdecl.prependParameter(pdecl)
          case (HasNode(n), TypeVariableHolder(tvDecl)) =>
            val genDecl = n.asInstanceOf[GenericElement]
            genDecl.addTypeParameter(tvDecl)
          case _ =>
            error(s"ContainsParam(${reenactor.getNode(e.container)}, ${reenactor.getNode(e.content)}) " +
              "should be between a decl and a param")
        }

      case Uses =>
        val (source, target ) = (resultGraph.getNode(e.source), resultGraph.getNode(e.target))
        (source.kind, target.kind) match {
          case (Class, Interface) =>
            logger.writeln("do not create %s : assuming its an isa edge (TOCHECK)".format(e)) // class imple
          case (Definition, Constructor) =>
            createUsesOfConstructor(resultGraph, reenactor, id2declMap, e)

          case (_: MethodKind, _ : TypeKind) =>
            val MethodDeclHolder(mdecl) = id2declMap(e.user)
            val TypedKindDeclHolder(tdecl) = id2declMap(e.target)
            mdecl.addException(tdecl.createLockedAccess())

          case (Definition, Field) => ()
            createUsesofField(resultGraph, reenactor, id2declMap, e)
          case (Definition, Method) if ensureIsInitalizerUseByCtor(reenactor, e)=>
            createInitializerCall(reenactor, id2declMap, e)

          case _ => logger.writeln(" =========> need to create " + e)
        }
      case _  =>
        logger.writeln(s"Creation of ${e.kind} ignored")

    }

  }

  def ensureIsInitalizerUseByCtor(graph: DependencyGraph, u : NodeIdP) : Boolean =
    graph.kindType(graph.container_!(u.user)) == TypeConstructor &&
      (graph.getRole(u.used) contains Initializer(graph.hostTypeDecl(u.user)))

  def createInitializerCall
  ( reenactor : DependencyGraph,
    id2declMap : NodeId => ASTNodeLink,
    e : NodeIdP)
  ( implicit program : Program, logger : PuckLogger) : Unit = {
    val sourceDecl = reenactor.container_!(e.user)
    (id2declMap(sourceDecl), id2declMap(e.used)) match {
      case (ConstructorDeclHolder(cdecl), MethodDeclHolder(mdecl)) =>
        if(cdecl.isImplicitConstructor) {
          cdecl.unsetImplicitConstructor()
          cdecl.hostType() match {
            case cd: ClassDecl =>
              cd.addBodyDecl(cdecl)
          }
        }
        cdecl.addInitializerCall(mdecl)
      case hs => error("createInitializerCall : expected constructor using method got " + hs)
    }
  }


  def createContains
  ( graph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap : NodeId => ASTNodeLink,
    e : DGEdge)
  ( implicit program : Program, logger : PuckLogger) : Unit =
    (id2declMap(e.container), id2declMap(e.content)) match {
      case (PackageDeclHolder, i: TypedKindDeclHolder) =>
        setPackageDecl(reenactor, e.container, e.content, i.decl)
        program.registerType(graph.fullName(e.content), i.decl)
      case (TypedKindDeclHolder(tdecl), MethodDeclHolder(mdecl)) =>
        tdecl.introduceMethod(mdecl)
        if(mdecl.isAbstract)
          tdecl.getModifiers.addModifier("abstract")

      case (TypedKindDeclHolder(tdecl), fdh @ FieldDeclHolder(fdecl, _)) =>
        //a field decl may contain several field declaration.
        import scala.collection.JavaConversions._
        if(!tdecl.fieldsIterator().contains(fdh.declarator))
          tdecl.introduceField(fdecl)

      case (_, PackageDeclHolder) => () // can be ignored

      case (ClassDeclHolder(clsdecl), bdHolder : HasBodyDecl) =>
        clsdecl.addBodyDecl(bdHolder.decl)

      case _ => logger.writeln(" =========> %s not created".format(e))

    }

  def createIsa
  (id2declMap: Map[NodeId, ASTNodeLink],
   subType : Type, supType : Type)
  ( implicit logger : PuckLogger,
    program : Program) : Unit = {

    val sub = id2declMap(Type mainId subType)
    val sup = id2declMap(Type mainId supType)

    val supAccess= createTypeAccess(id2declMap, supType)

    (sub, sup) match {
      case (ClassDeclHolder(subDecl), InterfaceDeclHolder(supDecl)) =>
        subDecl.addImplements(supAccess)
        subDecl.flushAttrCache()
      case (InterfaceDeclHolder(subDecl), InterfaceDeclHolder(supDecl)) =>
        subDecl.addSuperInterface(supAccess)
        subDecl.flushAttrCache()
      case (ClassDeclHolder(subDecl), ClassDeclHolder(supDecl)) =>
        subDecl.setSuperClass(supAccess)
        subDecl.flushAttrCache()
      case e => logger.writeln(s"isa($e) not created")
    }
  }

  def createUsesOfConstructor
  ( graph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap : NodeId => ASTNodeLink,
    e : NodeIdP)
  ( implicit logger : PuckLogger) : Unit = {
    val sourceDecl = reenactor declarationOf e.user
    val ConstructorDeclHolder(cdecl) = id2declMap(e.used)
    id2declMap(sourceDecl) match {
      case FieldDeclHolder(fdecl, idx)
        if fdecl.getDeclarator(idx).getInitOpt.isEmpty =>
        CreateNode.createNewInstanceExpr(fdecl.getDeclarator(idx), cdecl)
      case MethodDeclHolder(mdecl)
        if reenactor getRole sourceDecl contains Factory(e.used) =>
        mdecl.makeFactoryOf(cdecl)
      case dh =>
        import ShowDG._
        error(s"createUsesOfConstructor (${(reenactor, e).shows}) ${dh.getClass} " +
          s"with role ${reenactor getRole sourceDecl} as user unhandled")

    }
  }

  def createUsesofField
  (graph: DependencyGraph,
   reenactor : DependencyGraph,
   id2declMap : NodeId => ASTNodeLink,
   use : NodeIdP)
  ( implicit logger : PuckLogger) : Unit = {

    (id2declMap(use.user), id2declMap(use.used)) match {
      case (dh: DefHolder, fd @ FieldDeclHolder(_, _)) =>
        val userCter = reenactor container_! use.user
        val abss = reenactor.abstractions(use.used).filter {
          case ReadWriteAbstraction(rabs, wabs) =>
            rabs.contains(userCter) || wabs.contains(userCter)
          case _ => false
        }

        if(abss.size == 1) abss.head match {
          case ReadWriteAbstraction(Some(`userCter`), _) =>
            val MethodDeclHolder(mdecl) = id2declMap(userCter)
            mdecl.addGetterBody(fd.declarator)
          case ReadWriteAbstraction(_, Some(`userCter`)) =>
            val MethodDeclHolder(mdecl) = id2declMap(userCter)
            mdecl.addSetterBody(fd.declarator)
            mdecl.addGetterBody(fd.declarator)

          case _ => ()
        }
      case h => throw new puck.graph.Error(s"method decl and field decl expected, got $h")
    }
  }


  def setPackageDecl
  ( graph: DependencyGraph,
    packageId : NodeId,
    typeDeclNodeId : NodeId,
    td : TypeDecl)
  ( implicit program : Program, logger : PuckLogger) = {

    val cu = td.compilationUnit()
    val pkgDecl = graph.fullName(packageId)
    val path = ASTNodeLink.getPath(graph, packageId, typeDeclNodeId)
    cu.setPackageDecl(pkgDecl)
    cu.setPathName(path)
    //!\ very important !!
    cu.flushTreeCache()
  }

}
