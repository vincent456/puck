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

package puck
package jastadd
package concretize

import puck.graph.{TypeDecl => PTypeDecl, _}
import puck.javaGraph._
import puck.util.{PuckLog, PuckLogger}
import org.extendj.ast.{TypedKindDeclHolder, List => _, _}
import org.extendj.ast.JavaJastAddDG2AST.verbosity
import ShowDG._

object RedirectSource {

  def typesUsedInSubtree(graph : DependencyGraph, root : NodeId) : Seq[NodeId] =
    for {
      nid <- graph subTree root
      id <- graph usedBy nid
      if graph.kindType(id) == PTypeDecl
    } yield id

  def isAutoImported(t : TypeDecl) : Boolean = {
    t.isPrimitiveType || t.isString || t.isArrayDecl || t.isObject || t.isVoid
  }

  def removeImport
  ( cu: CompilationUnit, tDecl : TypeDecl)
  ( implicit logger : PuckLogger) : Unit = {
    logger.writeln(s"removeImportDecl of $tDecl in ${cu.pathName}")
    cu.removeImportDecl(tDecl)
  }
  def addImport
  ( cu: CompilationUnit, tDecl : TypeDecl)
  ( implicit logger : PuckLogger) : Unit = {
    val pa = new TypeAccess(tDecl.fullName())
    pa.lock(tDecl)
    if(!(tDecl.getVisibility == ASTNode.VIS_PUBLIC))
      tDecl.setVisibility(ASTNode.VIS_PUBLIC)
    logger.writeln(s"addImportDecl of $pa in ${cu.pathName}")
    cu.addImportDecl(new SingleTypeImportDecl(pa))

  }


  def moveMemberDecl
  ( reenactor : DependencyGraph,
    tDeclFrom : TypeDecl,
    tDeclDest : TypeDecl,
    hmd : HasMemberDecl,
    mDeclId : NodeId)
  ( implicit id2declMap: NodeId => ASTNodeLink,
    logger : PuckLogger): Unit = {


    hmd match {
      case FieldDeclHolder(fdecl, _) =>
        tDeclFrom.removeField(fdecl)
        tDeclDest.introduceField(fdecl)
      case MethodDeclHolder(mdecl) =>
        tDeclFrom.removeMethod(mdecl)
        tDeclDest.introduceMethod(mdecl)
    }

    ASTNodeLink.enlargeVisibility(
      reenactor, hmd.decl.asInstanceOf[Visible],
      mDeclId )

    if(tDeclFrom.compilationUnit() != tDeclDest.compilationUnit()){
      val typesUsed = typesUsedInSubtree(reenactor, mDeclId)

      import scala.collection.JavaConversions._
      val cu = tDeclDest.compilationUnit()
      val imports = tDeclDest.compilationUnit().getImportDecls.toList

      typesUsed.toSet[NodeId].foreach {
        tid =>
          typeDecl(reenactor, id2declMap, tid){
            t =>
              if(!isAutoImported(t))
                addImport(cu, t)
          }
      }
    }
  }




  def typeDecl
  (reenactor : DependencyGraph,
   id2declMap: NodeId => ASTNodeLink,
   typeId : NodeId)
  (f : TypeDecl => Unit) : Unit = {
    id2declMap(typeId) match {
      case TypedKindDeclHolder(t) => f(t)
      case NoDecl if reenactor.fullName(typeId) == "@primitive.[]" => ()
      case h => error(s"TypedKindDeclHolder expected got $h for ${reenactor.fullName(typeId)}")
    }
  }


  def fixImportOfMovedTypeDecl
  (reenactor : DependencyGraph,
   tDecl : TypeDecl,
   tDeclId : NodeId,
   oldPackage : String,
   newPackage : String,
   newlyCreatedCu : Boolean)
  ( implicit id2declMap: NodeId => ASTNodeLink,
    logger : PuckLogger): Unit = {

    val typesUsed = typesUsedInSubtree(reenactor, tDeclId)
    val cu = tDecl.compilationUnit()
    //3 cas
    //used est dans oldPackage -> ajouter import
    //used est dans newPackage -> enlever import
    //used est dans autre package -> remplacer import

    (typesUsed.toSet[NodeId] - tDeclId).foreach{ typeId =>
      typeDecl(reenactor, id2declMap, typeId){
        t =>
          if(!isAutoImported(t))
            t.compilationUnit().packageName() match {
              case `newPackage` => removeImport(cu, t)
              case `oldPackage` => addImport(cu, t)
              case _ if newlyCreatedCu => addImport(cu, t)
              case _ => // should be handled by the name locking
                //remove old import, create new properly named import
                addImport(cu, tDecl); removeImport(cu, tDecl)
            }
      }
    }
  }




  def fixImportForUsersOfMovedTypeDecl
  ( reenactor : DependencyGraph,
    tDecl : TypeDecl,
    tDeclId : NodeId,
    oldPackage : String,
    newPackage : String)
  ( implicit id2declMap: NodeId => ASTNodeLink,
    logger : PuckLogger): Unit = {

    def diffTypeDecl(td : TypeDecl) =
      if(td != tDecl) Some(td.compilationUnit())
      else None

    val staticContent = reenactor.content(tDeclId) filter (id => reenactor.kindType(id) match {
      case TypeConstructor | StaticValueDecl => true
      case _ => false})

    val impactedUsers =  (staticContent flatMap reenactor.usersOf) ++ (reenactor usersOf tDeclId)

    puck.ignore(impactedUsers.foldLeft(Set[String]()){ (cus, userId) =>
      val scu = id2declMap(userId) match {
        case ParameterDeclHolder(decl) =>
          diffTypeDecl(decl.hostType())
        case BlockHolder(blk) =>
          diffTypeDecl(blk.hostType())
        case ExprHolder(expr) =>
          diffTypeDecl(expr.hostType())
        case dh : HasBodyDecl =>
          diffTypeDecl(dh.decl.hostType())
        case tdh : TypedKindDeclHolder =>
          diffTypeDecl(tdh.decl)

        case dh => throw new DGError("should not happen, decl holder class is " + dh.getClass)
      }

      //3 cas
      //user est dans oldPackage -> ajouter import
      //user est dans newPackage -> enlever import
      //user est dans autre package -> remplacer import
      scu match {
        case Some(cu) if !cus.contains(cu.pathName()) =>

          cu.packageName() match {
            case `oldPackage` => addImport(cu, tDecl)
            case `newPackage` => removeImport(cu, tDecl)
            case _ =>  // should be handled by the name locking
              removeImport(cu, tDecl); addImport(cu, tDecl)
          }

          cus + cu.pathName
        case _ => cus
      }
    })

  }

  def moveNestedTypeKind
  ( resultGraph: DependencyGraph,
    reenactor : DependencyGraph,
    oldContainer : NodeId,
    newContainer: NodeId,
    tDecl : TypeDecl,
    tDeclId : NodeId)
  ( implicit program : Program,
    id2declMap: NodeId => ASTNodeLink,
    logger : PuckLogger) : Unit = {
    logger.writeln("moving " + tDecl.fullName() + " to  " + resultGraph.fullName(newContainer))
    ???
  }

  def moveTopLevelTypeKind
  ( resultGraph: DependencyGraph,
    reenactor : DependencyGraph,
    oldPackage : NodeId,
    newPackage: NodeId,
    tDecl : TypeDecl,
    tDeclId : NodeId)
  ( implicit program : Program,
    id2declMap: NodeId => ASTNodeLink,
    logger : PuckLogger) : Unit = {
    logger.writeln("moving " + tDecl.fullName() +" to package " + resultGraph.fullName(newPackage))


    var newlyCreatedCu = false

    if (tDecl.compilationUnit.getNumTypeDecl > 1) {
      logger.writeln(tDecl.name + " cu with more than one classe")(verbosity(PuckLog.Debug))
      newlyCreatedCu = true
      logger.writeln(tDecl.program().prettyPrint())
      val path = ASTNodeLink.getPath(reenactor, newPackage)
      val oldcu = tDecl.compilationUnit()

      oldcu.removeTypeDecl(tDecl)
      val newCu = program.insertUnusedType(path, resultGraph.fullName(newPackage), tDecl)

      newCu.setPathName(tDecl.fullName().replaceAllLiterally(".", "/") + ".java")

    }
    else {
      logger.writeln(tDecl.name + " cu with one classe")(verbosity(PuckLog.Debug))
      logger.writeln("before " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))
      CreateEdge.setPackageDecl(resultGraph, newPackage, tDeclId, tDecl)
      logger.writeln("after " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))
    }

    fixVisibilityAndImports(resultGraph, reenactor,
      reenactor fullName oldPackage, reenactor fullName newPackage,
      tDecl, tDeclId, newlyCreatedCu)

  }

  def fixVisibilityAndImports
  (resultGraph: DependencyGraph,
   reenactor : DependencyGraph,

   oldPackageFullName : String,
   newPackageFullName : String,
   tDecl : TypeDecl,
   tDeclId : NodeId,
   newlyCreatedCu : Boolean)
  ( implicit program : Program,
    id2declMap: NodeId => ASTNodeLink,
    logger : PuckLogger) : Unit= {
    ASTNodeLink.enlargeVisibility(resultGraph, tDecl, tDeclId)
    tDecl.flushCache()
    //val oldFullName = resultGraph.fullName(oldPackage) + "." + tDecl.name()
    val oldFullName = reenactor.fullName(tDeclId)
    program.changeTypeMap(oldFullName, resultGraph.fullName(tDeclId), tDecl)


    fixImportForUsersOfMovedTypeDecl(reenactor, tDecl, tDeclId, oldPackageFullName, newPackageFullName)
    fixImportOfMovedTypeDecl(reenactor, tDecl, tDeclId, oldPackageFullName, newPackageFullName, newlyCreatedCu)

    val staticMemberType = reenactor.content(tDeclId) filter
      (nid => (reenactor kindType nid) == PTypeDecl)
    staticMemberType.foreach{ id =>
      id2declMap(id) match {
        case t : TypedKindDeclHolder =>
          fixVisibilityAndImports(resultGraph, reenactor,
            oldPackageFullName, newPackageFullName, t.decl, id, newlyCreatedCu)
        case t => error(s"TypedKindDeclHolder expected but got ${t.getClass} ")
      }

    }
  }




  def move(source : NodeId, target : NodeId, newSource : NodeId)
          ( implicit program : Program,
            logger : PuckLogger,
            resultAndReenactor : (DependencyGraph, DependencyGraph),
            id2declMap: NodeId => ASTNodeLink ) : Unit = {
    val (resultGraph, reenactor) = resultAndReenactor
    (id2declMap(source), id2declMap(newSource), id2declMap(target)) match {
      case (TypedKindDeclHolder(oldTdecl),
      TypedKindDeclHolder(newTdecl),
      bdh : HasMemberDecl) =>
        moveMemberDecl(reenactor, oldTdecl, newTdecl, bdh, target)
        if(bdh.decl.isStatic) {
          reenactor.usersOf(target).foreach {
            id2declMap(_) match {
              case hn : HasNode =>
                hn.node.handleQualifierOfAccessOfStaticMember(bdh.decl, newTdecl)
              case _ => puck.error()
            }
          }

          val usedBy = ((reenactor.content(target) + target) flatMap reenactor.usedBy) - target

          ( usedBy intersect reenactor.content(source)).foreach {
            id2declMap(_) match {
              case FieldDeclHolder(fdecl, i) =>
                bdh.decl.handleQualifierOfAccessOfStaticMember(fdecl.getDeclarator(i), oldTdecl)
              case MethodDeclHolder(mdecl) =>
                bdh.decl.handleQualifierOfAccessOfStaticMember(mdecl, oldTdecl)
              case ConstructorDeclHolder(cdecl) =>
                bdh.decl.handleQualifierOfAccessOfStaticMember(cdecl, oldTdecl)
              case h => puck.error(h + " not expected")
            }
          }
        }


      case (_, _, TypedKindDeclHolder(tDecl)) =>
        if(tDecl.isTopLevelType)
          moveTopLevelTypeKind(resultGraph, reenactor, source, newSource, tDecl, target)
        else if(tDecl.isStatic){
          moveNestedTypeKind(resultGraph, reenactor, source, newSource, tDecl, target)
        }
        else throw new PuckError("Moving inner (non static) type not handled")


      case (PackageDeclHolder, PackageDeclHolder, PackageDeclHolder) =>
        reenactor.subTree(source).filter{
          n =>
            reenactor.kindType(n) == PTypeDecl &&
              (reenactor kindType reenactor.container_!(n)) == NameSpace // only top level types
        } map (n => (reenactor container_! n, n) ) foreach {
          case (p, t) =>
            val TypedKindDeclHolder(tdecl) = id2declMap(t)
            CreateEdge.setPackageDecl(resultGraph, p, t, tdecl)
            val packageFullName = resultGraph fullName p
            fixVisibilityAndImports(resultGraph, reenactor,
              packageFullName, packageFullName, tdecl, t,
              newlyCreatedCu = false)
        }

      case _ =>
        val eStr = (reenactor, Contains(source, target)).shows
        val nsrcStr = (reenactor, newSource).shows(desambiguatedFullName)
        throw new JavaAGError(s"redirecting SOURCE of $eStr to $nsrcStr : application failure !")
    }
  }

  def redirectIsaSource(source : NodeId, target : NodeId, newSource : NodeId)
                       ( implicit program : Program,
                         logger : PuckLogger,
                         id2declMap: NodeId => ASTNodeLink ) : Unit = {
    removeIsa(id2declMap(source), id2declMap(target))
    CreateEdge.createIsa(id2declMap(newSource), id2declMap(target))
  }

  def changeUser(source : NodeId, target : NodeId, newSource : NodeId)
                ( implicit program : Program,
                  logger : PuckLogger,
                  resultAndReenactor : (DependencyGraph, DependencyGraph),
                  id2declMap: NodeId => ASTNodeLink ) : Unit = {
    val (_, reenactor) = resultAndReenactor

    val newSrcDecl = reenactor container_! newSource
    reenactor getRole newSrcDecl match {
      case Some(Initializer(_)) =>
        moveFieldInitialization(reenactor, id2declMap, source, newSrcDecl)
      case Some(Factory(_)) =>
        assert( reenactor getRole target match {
          case Some(Initializer(_)) => true
          case _ => false
        })
        moveInitFromCtorToFactory(reenactor, id2declMap, target, source, newSrcDecl)
      case sr => error(s"Redirect source of use, expecting new user to be some initializer," +
        s" ${reenactor getConcreteNode newSrcDecl} is $sr ")
    }
  }

  def removeIsa
  ( sub : ASTNodeLink, sup : ASTNodeLink)
  ( implicit logger : PuckLogger) : Unit = (sub,sup) match {
    case (sub : TypedKindDeclHolder, sup: TypedKindDeclHolder) =>
      sub.decl.removeSuperType(sup.decl)

    case e => logger.writeln(s"isa($e) not deleted")
  }


  def moveFieldInitialization
  (reenactor : DependencyGraph,
   id2declMap: NodeId => ASTNodeLink,
   oldSource : NodeId,
   newSourceDecl : NodeId) : Unit = {

    (id2declMap(reenactor container_! oldSource),
      id2declMap(newSourceDecl)) match {
      case (FieldDeclHolder(fdecl,num), MethodDeclHolder(mdecl)) =>
        fdecl.getDeclarator(num).moveInitIntoInitializzer(mdecl)
      case hs =>
        error(s"Redirect source of use handled in case of initializer creation, $hs not expected")
    }
  }
  def moveInitFromCtorToFactory
  ( reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    initializerDeclId : NodeId,
    ctorDefId : NodeId,
    factoryDeclId : NodeId) : Unit = {

    (id2declMap(reenactor container_! ctorDefId),
      id2declMap(factoryDeclId),
      id2declMap(initializerDeclId)) match {
      case (ConstructorDeclHolder(cdecl),
      MethodDeclHolder(mdecl),
      MethodDeclHolder(init)) =>
        cdecl.removeInitCall(init)
        mdecl.createInitializerCall(init)
      case hs =>
        error(s"Redirect source of use handled in case of initializer creation, $hs not expected")
    }

  }
}
