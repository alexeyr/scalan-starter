package scalan.examples
package impl

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe._
import scalan.common.Default

// Abs -----------------------------------
trait MyArraysAbs extends Scalan with MyArrays {
  self: ExampleDsl =>
  // single proxy for each type family
  implicit def proxyMyArray[A](p: Rep[MyArray[A]]): MyArray[A] = {
    implicit val tag = weakTypeTag[MyArray[A]]
    proxyOps[MyArray[A]](p)(TagImplicits.typeTagToClassTag[MyArray[A]])
  }

  abstract class MyArrayElem[A, From, To <: MyArray[A]](iso: Iso[From, To])(implicit elem: Elem[A])
    extends EntityElem[To] { //extends ViewElem[From, To](iso) {
    override def convert(x: Rep[Def[_]]) = convertMyArray(x.asRep[MyArray[A]])
    def convertMyArray(x : Rep[MyArray[A]]): Rep[To]
  }

  implicit case object MyArrayCompanionElem extends CompanionElem[MyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[MyArrayCompanionAbs]
    protected def getDefaultRep = MyArray
  }

  abstract class MyArrayCompanionAbs extends CompanionDef[MyArrayCompanionAbs] with MyArrayCompanion {
    def selfType = MyArrayCompanionElem
    override def toString = "MyArray"
  }

  def MyArray: Rep[MyArrayCompanionAbs]
  implicit def proxyMyArrayCompanion(p: Rep[MyArrayCompanion]): MyArrayCompanion = {
    proxyOps[MyArrayCompanion](p)
  }

  abstract class AbsBaseMyArray[A]
    (override val values: Rep[Collection[A]])
    (implicit eA: Elem[A])
    extends BaseMyArray[A](values) with Def[BaseMyArray[A]] {
    lazy val selfType = element[BaseMyArray[A]]
  }  

  // elem for concrete class
  class BaseMyArrayElem[A](iso: Iso[BaseMyArrayData[A], BaseMyArray[A]])(implicit val eA: Elem[A])
    extends MyArrayElem[A, BaseMyArrayData[A], BaseMyArray[A]](iso) {
    def convertMyArray(x: Rep[MyArray[A]]) = BaseMyArray(x.values)
  }

  // state representation type
  type BaseMyArrayData[A] = Collection[A]

  // 3) Iso for concrete class
  class BaseMyArrayIso[A](implicit eA: Elem[A])
    extends Iso[BaseMyArrayData[A], BaseMyArray[A]] {
    override def from(p: Rep[BaseMyArray[A]]) =
      unmkBaseMyArray(p) match {
        case Some((values)) => values
        case None => !!!
      }
    override def to(p: Rep[Collection[A]]) = {
      val values = p
      BaseMyArray(values)
    }
    lazy val tag = {
      weakTypeTag[BaseMyArray[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[BaseMyArray[A]]](BaseMyArray(element[Collection[A]].defaultRepValue))
    lazy val eTo = new BaseMyArrayElem[A](this)
  }
  // 4) constructor and deconstructor
  implicit case object BaseMyArrayCompanionElem extends CompanionElem[BaseMyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[BaseMyArrayCompanionAbs]
    protected def getDefaultRep = BaseMyArray
  }

  abstract class BaseMyArrayCompanionAbs extends CompanionDef[BaseMyArrayCompanionAbs] with BaseMyArrayCompanion {
    def selfType = BaseMyArrayCompanionElem
    override def toString = "BaseMyArray"

    def apply[A](values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]] =
      mkBaseMyArray(values)
    def unapply[A:Elem](p: Rep[BaseMyArray[A]]) = unmkBaseMyArray(p)
  }
  def BaseMyArray: Rep[BaseMyArrayCompanionAbs]
  implicit def proxyBaseMyArrayCompanion(p: Rep[BaseMyArrayCompanionAbs]): BaseMyArrayCompanionAbs = {
    proxyOps[BaseMyArrayCompanionAbs](p)
  }

  implicit def proxyBaseMyArray[A](p: Rep[BaseMyArray[A]]): BaseMyArray[A] =
    proxyOps[BaseMyArray[A]](p)

  implicit class ExtendedBaseMyArray[A](p: Rep[BaseMyArray[A]])(implicit eA: Elem[A]) {
    def toData: Rep[BaseMyArrayData[A]] = isoBaseMyArray(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseMyArray[A](implicit eA: Elem[A]): Iso[BaseMyArrayData[A], BaseMyArray[A]] =
    new BaseMyArrayIso[A]

  // 6) smart constructor and deconstructor
  def mkBaseMyArray[A](values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]]
  def unmkBaseMyArray[A:Elem](p: Rep[BaseMyArray[A]]): Option[(Rep[Collection[A]])]

  // elem for concrete class
  class PairMyArrayElem[A, B](iso: Iso[PairMyArrayData[A, B], PairMyArray[A, B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends MyArrayElem[(A,B), PairMyArrayData[A, B], PairMyArray[A, B]](iso) {
    def convertMyArray(x: Rep[MyArray[(A,B)]]) = PairMyArray(x.values)
  }

  abstract class AbsPairMyArray[A, B]
      (override val values: Rep[Collection[(A,B)]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairMyArray[A, B](values) with Def[PairMyArray[A, B]] {
    lazy val selfType = element[PairMyArray[A, B]].asInstanceOf[Elem[MyArray[(A,B)]]]
  }

  // state representation type
  type PairMyArrayData[A, B] = Collection[(A,B)]

  // 3) Iso for concrete class
  class PairMyArrayIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[PairMyArrayData[A, B], PairMyArray[A, B]] {
    override def from(p: Rep[PairMyArray[A, B]]) =
      unmkPairMyArray(p) match {
        case Some((values)) => values
        case None => !!!
      }
    override def to(p: Rep[Collection[(A,B)]]) = {
      val values = p
      PairMyArray(values)
    }
    lazy val tag = {
      weakTypeTag[PairMyArray[A, B]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[PairMyArray[A, B]]](PairMyArray(element[Collection[(A,B)]].defaultRepValue))
    lazy val eTo = new PairMyArrayElem[A, B](this)
  }
  // 4) constructor and deconstructor
  implicit case object PairMyArrayCompanionElem extends CompanionElem[PairMyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[PairMyArrayCompanionAbs]
    protected def getDefaultRep = PairMyArray
  }
  
  abstract class PairMyArrayCompanionAbs extends CompanionDef[PairMyArrayCompanionAbs] with PairMyArrayCompanion {
    def selfType = PairMyArrayCompanionElem
    override def toString = "PairMyArray"

    def apply[A, B](values: Rep[Collection[(A,B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
      mkPairMyArray(values)
    def unapply[A:Elem, B:Elem](p: Rep[PairMyArray[A, B]]) = unmkPairMyArray(p)
  }
  def PairMyArray: Rep[PairMyArrayCompanionAbs]
  implicit def proxyPairMyArrayCompanion(p: Rep[PairMyArrayCompanionAbs]): PairMyArrayCompanionAbs = {
    proxyOps[PairMyArrayCompanionAbs](p)
  }

  implicit def proxyPairMyArray[A, B](p: Rep[PairMyArray[A, B]]): PairMyArray[A, B] =
    proxyOps[PairMyArray[A, B]](p)

  implicit class ExtendedPairMyArray[A, B](p: Rep[PairMyArray[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[PairMyArrayData[A, B]] = isoPairMyArray(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairMyArray[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairMyArrayData[A, B], PairMyArray[A, B]] =
    new PairMyArrayIso[A, B]

  // 6) smart constructor and deconstructor
  def mkPairMyArray[A, B](values: Rep[Collection[(A,B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]]
  def unmkPairMyArray[A:Elem, B:Elem](p: Rep[PairMyArray[A, B]]): Option[(Rep[Collection[(A,B)]])]
}

// Seq -----------------------------------
trait MyArraysSeq extends MyArraysDsl with ScalanSeq {
  self: ExampleDslSeq =>
  lazy val MyArray: Rep[MyArrayCompanionAbs] = new MyArrayCompanionAbs{}

  case class SeqBaseMyArray[A]
      (override val values: Rep[Collection[A]])
      (implicit eA: Elem[A])
    extends BaseMyArray[A](values) {
    lazy val selfType = element[BaseMyArray[A]].asInstanceOf[Elem[MyArray[A]]]
  }
  lazy val BaseMyArray = new BaseMyArrayCompanionAbs {}

  def mkBaseMyArray[A]
      (values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]] =
      new SeqBaseMyArray[A](values)
  def unmkBaseMyArray[A:Elem](p: Rep[BaseMyArray[A]]) =
    Some((p.values))

  case class SeqPairMyArray[A, B]
      (override val values: Rep[Collection[(A,B)]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairMyArray[A, B](values) {
    lazy val selfType = element[PairMyArray[A, B]].asInstanceOf[Elem[MyArray[(A,B)]]]
  }
  lazy val PairMyArray = new PairMyArrayCompanionAbs{}

  def mkPairMyArray[A, B]
      (values: Rep[Collection[(A,B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
      new SeqPairMyArray[A, B](values)
  def unmkPairMyArray[A:Elem, B:Elem](p: Rep[PairMyArray[A, B]]) =
    Some((p.values))
}

// Exp -----------------------------------
trait MyArraysExp extends MyArraysDsl with ScalanExp {
  self: ExampleDslExp =>
  lazy val MyArray: Rep[MyArrayCompanionAbs] = new MyArrayCompanionAbs {}

/*
  case class ExpBaseMyArray[A]
      (override val values: Rep[Collection[A]])
      (implicit eA: Elem[A])
    extends BaseMyArray[A](values) {
    lazy val selfType = element[BaseMyArray[A]].asInstanceOf[Elem[MyArray[A]]]
    override def mirror(t: Transformer) = ExpBaseMyArray[A](t(values))
  }
*/

  case class ExpBaseMyArray[A]
      (override val values: Rep[Collection[A]])
      (implicit eA: Elem[A])
    extends AbsBaseMyArray[A](values)(eA) {}

  lazy val BaseMyArray: Rep[BaseMyArrayCompanionAbs] = new BaseMyArrayCompanionAbs{}
  object BaseMyArrayMethods {
    object elem {
      def unapply(d: Def[_]): Option[Rep[BaseMyArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BaseMyArrayElem[_]] && method.getName == "elem" =>
          Some(receiver).asInstanceOf[Option[Rep[BaseMyArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BaseMyArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[BaseMyArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BaseMyArrayElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[BaseMyArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BaseMyArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[BaseMyArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[BaseMyArrayElem[_]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[BaseMyArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseMyArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BaseMyArrayCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Elem[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(ea, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_,_,_]] && method.getName == "defaultOf" =>
          Some(ea).asInstanceOf[Option[Elem[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Elem[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkBaseMyArray[A]
    (values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]] =
    new ExpBaseMyArray[A](values)
  def unmkBaseMyArray[A:Elem](p: Rep[BaseMyArray[A]]) =
    Some((p.values))
/*
  case class ExpPairMyArray[A, B]
      (override val values: Rep[Collection[(A,B)]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairMyArray[A, B](values) {
    lazy val selfType = element[PairMyArray[A, B]].asInstanceOf[Elem[MyArray[(A,B)]]]
    override def mirror(t: Transformer) = ExpPairMyArray[A, B](t(values))
  }
*/
  case class ExpPairMyArray[A, B]
      (override val values: Rep[Collection[(A,B)]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends AbsPairMyArray[A, B](values)(eA, eB) {}

  lazy val PairMyArray: Rep[PairMyArrayCompanionAbs] = new PairMyArrayCompanionAbs {}

  object PairMyArrayMethods {
    object arr {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object as {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "as" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bs {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "bs" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairMyArray[A, B]], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[PairMyArray[A, B]], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairMyArray[A, B]], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairMyArrayCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[(Elem[A], Elem[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ea, eb, _*), _) if receiver.elem.isInstanceOf[PairMyArray[_,_]] && method.getName == "defaultOf" =>
          Some((ea, eb)).asInstanceOf[Option[(Elem[A], Elem[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Elem[A], Elem[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `apply`: Method's return type PairMyArray[A,B] is not a Rep
  }

  def mkPairMyArray[A, B]
    (values: Rep[Collection[(A,B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
    new ExpPairMyArray[A, B](values)
  def unmkPairMyArray[A:Elem, B:Elem](p: Rep[PairMyArray[A, B]]) =
    Some((p.values))

  object MyArrayMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[MyArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MyArrayElem[_, _, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[MyArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MyArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object values {
      def unapply(d: Def[_]): Option[Rep[MyArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MyArrayElem[_, _, _]] && method.getName == "values" =>
          Some(receiver).asInstanceOf[Option[Rep[MyArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MyArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[MyArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[MyArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MyArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[MyArray[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[MyArray[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MyArray[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[MyArray[A]], MyArr[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[MyArray[A]], MyArr[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MyArray[A]], MyArr[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MyArrayCompanionMethods {
    // WARNING: Cannot generate matcher for method `defaultOf`: Method's return type Default[Rep[MyArray[A]]] is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem.isInstanceOf[MyArray[_]] && method.getName == "apply" =>
          Some(arr).asInstanceOf[Option[Rep[Collection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem.isInstanceOf[MyArray[_]] && method.getName == "fromArray" =>
          Some(arr).asInstanceOf[Option[Rep[Collection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Option[(Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(len, v, _*), _) if receiver.elem.isInstanceOf[MyArray[_]] && method.getName == "replicate" =>
          Some((len, v)).asInstanceOf[Option[(Rep[Int], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
