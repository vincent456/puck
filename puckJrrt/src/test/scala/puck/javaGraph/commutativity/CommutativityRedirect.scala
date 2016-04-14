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

package puck.javaGraph.commutativity

import puck.javaGraph.nodeKind.Field
import puck.{AcceptanceSpec, QuickFrame, Settings}
import puck.graph.comparison.Mapping
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.graph.{AccessAbstraction, Factory, Uses}
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, Redirection}
import puck.javaGraph.{JavaDotHelper, ScenarioFactory}
import puck.Settings.outDir

class CommutativityRedirect
  extends AcceptanceSpec {

  val examplesPath = Settings.testExamplesPath + "/redirection/"

  feature("TypeDecl uses redirection") {

    scenario("From class to interface superType") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |class ClassUsed implements SuperType{ public void mUsed(){} }
          |
          |interface SuperType { void mUsed(); }
          |
          |class A {
          |
          |    public static void main(String[] args){
          |        A a = new A();
          |        a.mUser(new ClassUsed());
          |    }
          |
          |    void mUser(ClassUsed cu){ cu.mUsed(); }
          |}"""
      ) {
        val theParam = fullName2id("p.A.mUser(ClassUsed).cu")

        val classUsed = fullName2id("p.ClassUsed")
        val superType = fullName2id("p.SuperType")

        val g =
          Redirection.redirectUsesAndPropagate(graph,
            Uses(theParam, classUsed),
            AccessAbstraction(superType, SupertypeAbstraction)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert(Mapping.equals(g, recompiledEx.graph))

      }
    }

    ignore("From class to class superType"){}

    ignore("From interface to interface superType"){}

    ignore("From class to delegator class") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |class Delegatee { void mUsed(){} }
          |
          |class Delegator {
          |    Delegatee d;
          |    void mUsed(){ d.mUsed(); }
          |}
          |
          |class A {
          |
          |    public static void main(String[] args){
          |        A a = new A();
          |        a.mUser(new Delegatee());
          |    }
          |
          |    void mUser(Delegatee d){ d.mUsed(); }
          |}"""
      ) {
        val theParam = fullName2id("p.A.mUser(Delegatee).d")

        val delegatee = fullName2id("p.Delegatee")
        val mDelegatee = fullName2id("p.Delegatee.mUsed()")

        val delegator = fullName2id("p.Delegator")
        val mDelegator = fullName2id("p.Delegator.mUsed()")

        //QuickFrame(graph, "g", JavaDotHelper)

        val g = graph.addAbstraction(delegatee, AccessAbstraction(delegator, DelegationAbstraction))
          .addAbstraction(mDelegatee, AccessAbstraction(mDelegator, DelegationAbstraction))

        val g2 =
          Redirection.redirectUsesAndPropagate(g,
            Uses(theParam, delegatee),
            AccessAbstraction(delegator, DelegationAbstraction)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))
      }

    }

    ignore("From interface to delegator class") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |interface I { void mUsed(); }
          |
          |class Impl implements I { public void mUsed(){} }
          |
          |class Delegator {
          |    I d;
          |    void mUsed(){ d.mUsed(); }
          |}
          |
          |class A {
          |
          |    public static void main(String[] args){
          |        A a = new A();
          |        a.mUser(new Impl());
          |    }
          |
          |    void mUser(I i){ i.mUsed(); }
          |}"""
      ){

      }
    }

  }

  feature("TypeConstructor uses redirection") {


    scenario("From constructor to constructorMethod hosted elsewhere - non static, parameter") {
      val _ = new ScenarioFactory(
          """package p;
            |
            |class Factory{
            |    Factory(){}
            |    B createB(){ return new B(); }
            |}
            |
            |class B { B(){} }
            |
            |class A {
            |    void m() { B b = new B(); }
            |}"""
          ){
        val ctor = fullName2id("p.B.B()")
        val ctorMethod = fullName2id("p.Factory.createB()")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))
                    .setRole(ctorMethod, Some(Factory(ctor)))

        val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).rvalue

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)
        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

    scenario("From constructor to constructorMethod hosted elsewhere - non static, field") {
      val _ = new ScenarioFactory(
          """package p;
            |
            |class Factory{
            |    Factory(){}
            |    B createB(){ return new B(); }
            |}
            |
            |class B { B(){} }
            |
            |class A {
            |    void m() { B b = new B(); }
            |}"""
      ) {
        val ctor = fullName2id("p.B.B()")
        val ctorMethod = fullName2id("p.Factory.createB()")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))
                      .setRole(ctorMethod, Some(Factory(ctor)))

        val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).rvalue


        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

    ignore("From constructor to constructorMethod hosted by self - non static, parameter") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |class B {
          |    B(){}
          |    B create(){return new B();}
          |}
          |
          |class A { void m(){ new B(); } }
          |
          |class C {
          |    void mc(){
          |        A a = new A();
          |        a.m();
          |    }
          |}"""
      ) {
        val ctor = fullName2id("p.B.B()")
        val ctorMethod = fullName2id("p.B.create()")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        println(s"ctor = $ctor")
        println(s"ctorMethod = $ctorMethod")
        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))
                      .setRole(ctorMethod, Some(Factory(ctor)))
        //QuickFrame(g, "g", JavaDotHelper)
        val g2 =
          Redirection.redirectTypeConstructorToInstanceValueDecl(g,
            Uses(callerDef, ctor),
            AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).rvalue

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))

      }
    }

    scenario("From constructor to constructorMethod hosted by self - non static, field") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |class B {
          |    B(){}
          |    B create(){return new B();}
          |}
          |
          |class A { void m(){ new B(); } }
          |
          |class C {
          |    void mc(){
          |        A a = new A();
          |        a.m();
          |    }
          |}"""
      ) {
        val ctor = fullName2id("p.B.B()")
        val ctorMethod = fullName2id("p.B.create()")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectTypeConstructorToInstanceValueDecl(g,
            Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)
        assert(Mapping.equals(g2, recompiledEx.graph))

      }
    }
  }

  feature("TypeMember uses redirection"){

        val typeMemberPath = examplesPath + "typeMember"

        scenario("From method to method superType"){
          val _ = new ScenarioFactory(
            """package p;
              |
              |interface B {
              |    void m1();
              |    void m2();
              |}
              |
              |class Bimpl implements B {
              |    public void m1(){}
              |    public void m2(){}
              |}
              |
              |class A {
              |    void m(){
              |        Bimpl b = new Bimpl();
              |        b.m1();
              |        b.m2();
              |    }
              |}""") {
            val `p.Bimpl.m1()` = fullName2id("p.Bimpl.m1()")
            val `p.B.m1()` = fullName2id("p.B.m1()")

            val `p.A.m().Definition` = fullName2id("p.A.m().Definition")


            val g =
              Redirection.redirectUsesAndPropagate(graph,
                Uses(`p.A.m().Definition`, `p.Bimpl.m1()`),
                AccessAbstraction(`p.B.m1()`, SupertypeAbstraction)).rvalue

            val recompiledEx = applyChangeAndMakeExample(g, outDir)
            assert( Mapping.equals(g, recompiledEx.graph) )


          }
        }

        ignore("From method to method delegate"){

        }

        ignore("From field to ??? delegate"){
          //what should we do ?
        }


  }
}
