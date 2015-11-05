package scalan.examples

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe._

package impl {
// Abs -----------------------------------
trait MyArraysAbs extends Scalan with MyArrays {
  self: ExampleDsl =>

  // single proxy for each type family
  implicit def proxyMyArray[A](p: Rep[MyArray[A]]): MyArray[A] = {
    proxyOps[MyArray[A]](p)(scala.reflect.classTag[MyArray[A]])
  }

  // familyElem
  class MyArrayElem[A, To <: MyArray[A]](implicit _elem: Elem[A])
    extends EntityElem[To] {
    def elem = _elem
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(elem))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = elem.tag
      weakTypeTag[MyArray[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[MyArray[A]] => convertMyArray(x) }
      tryConvert(element[MyArray[A]], this, x, conv)
    }

    def convertMyArray(x: Rep[MyArray[A]]): Rep[To] = {
      x.selfType1 match {
        case _: MyArrayElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have MyArrayElem[_, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def myArrayElement[A](implicit elem: Elem[A]): Elem[MyArray[A]] =
    cachedElem[MyArrayElem[A, MyArray[A]]](elem)

  implicit case object MyArrayCompanionElem extends CompanionElem[MyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[MyArrayCompanionAbs]
    protected def getDefaultRep = MyArray
  }

  abstract class MyArrayCompanionAbs extends CompanionDef[MyArrayCompanionAbs] with MyArrayCompanion {
    def selfType = MyArrayCompanionElem
    override def toString = "MyArray"
  }
  def MyArray: Rep[MyArrayCompanionAbs]
  implicit def proxyMyArrayCompanion(p: Rep[MyArrayCompanion]): MyArrayCompanion =
    proxyOps[MyArrayCompanion](p)

  abstract class AbsBaseMyArray[A]
      (values: Rep[Collection[A]])(implicit eA: Elem[A])
    extends BaseMyArray[A](values) with Def[BaseMyArray[A]] {
    lazy val selfType = element[BaseMyArray[A]]
  }
  // elem for concrete class
  class BaseMyArrayElem[A](val iso: Iso[BaseMyArrayData[A], BaseMyArray[A]])(implicit eA: Elem[A])
    extends MyArrayElem[A, BaseMyArray[A]]
    with ConcreteElem[BaseMyArrayData[A], BaseMyArray[A]] {
    override lazy val parent: Option[Elem[_]] = Some(myArrayElement(element[A]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }

    override def convertMyArray(x: Rep[MyArray[A]]) = BaseMyArray(x.values)
    override def getDefaultRep = BaseMyArray(element[Collection[A]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[BaseMyArray[A]]
    }
  }

  // state representation type
  type BaseMyArrayData[A] = Collection[A]

  // 3) Iso for concrete class
  class BaseMyArrayIso[A](implicit eA: Elem[A])
    extends Iso[BaseMyArrayData[A], BaseMyArray[A]] {
    override def from(p: Rep[BaseMyArray[A]]) =
      p.values
    override def to(p: Rep[Collection[A]]) = {
      val values = p
      BaseMyArray(values)
    }
    lazy val eTo = new BaseMyArrayElem[A](this)
  }
  // 4) constructor and deconstructor
  class BaseMyArrayCompanionAbs extends CompanionDef[BaseMyArrayCompanionAbs] with BaseMyArrayCompanion {
    def selfType = BaseMyArrayCompanionElem
    override def toString = "BaseMyArray"

    def apply[A](values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]] =
      mkBaseMyArray(values)
  }
  object BaseMyArrayMatcher {
    def unapply[A](p: Rep[MyArray[A]]) = unmkBaseMyArray(p)
  }
  lazy val BaseMyArray: Rep[BaseMyArrayCompanionAbs] = new BaseMyArrayCompanionAbs
  implicit def proxyBaseMyArrayCompanion(p: Rep[BaseMyArrayCompanionAbs]): BaseMyArrayCompanionAbs = {
    proxyOps[BaseMyArrayCompanionAbs](p)
  }

  implicit case object BaseMyArrayCompanionElem extends CompanionElem[BaseMyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[BaseMyArrayCompanionAbs]
    protected def getDefaultRep = BaseMyArray
  }

  implicit def proxyBaseMyArray[A](p: Rep[BaseMyArray[A]]): BaseMyArray[A] =
    proxyOps[BaseMyArray[A]](p)

  implicit class ExtendedBaseMyArray[A](p: Rep[BaseMyArray[A]])(implicit eA: Elem[A]) {
    def toData: Rep[BaseMyArrayData[A]] = isoBaseMyArray(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseMyArray[A](implicit eA: Elem[A]): Iso[BaseMyArrayData[A], BaseMyArray[A]] =
    cachedIso[BaseMyArrayIso[A]](eA)

  // 6) smart constructor and deconstructor
  def mkBaseMyArray[A](values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]]
  def unmkBaseMyArray[A](p: Rep[MyArray[A]]): Option[(Rep[Collection[A]])]

  abstract class AbsPairMyArray[A, B]
      (values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B])
    extends PairMyArray[A, B](values) with Def[PairMyArray[A, B]] {
    lazy val selfType = element[PairMyArray[A, B]]
  }
  // elem for concrete class
  class PairMyArrayElem[A, B](val iso: Iso[PairMyArrayData[A, B], PairMyArray[A, B]])(implicit eA: Elem[A], eB: Elem[B])
    extends MyArrayElem[(A, B), PairMyArray[A, B]]
    with ConcreteElem[PairMyArrayData[A, B], PairMyArray[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(myArrayElement(pairElement(element[A],element[B])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertMyArray(x: Rep[MyArray[(A, B)]]) = PairMyArray(x.values)
    override def getDefaultRep = PairMyArray(element[Collection[(A, B)]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[PairMyArray[A, B]]
    }
  }

  // state representation type
  type PairMyArrayData[A, B] = Collection[(A, B)]

  // 3) Iso for concrete class
  class PairMyArrayIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[PairMyArrayData[A, B], PairMyArray[A, B]] {
    override def from(p: Rep[PairMyArray[A, B]]) =
      p.values
    override def to(p: Rep[Collection[(A, B)]]) = {
      val values = p
      PairMyArray(values)
    }
    lazy val eTo = new PairMyArrayElem[A, B](this)
  }
  // 4) constructor and deconstructor
  class PairMyArrayCompanionAbs extends CompanionDef[PairMyArrayCompanionAbs] with PairMyArrayCompanion {
    def selfType = PairMyArrayCompanionElem
    override def toString = "PairMyArray"

    def apply[A, B](values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
      mkPairMyArray(values)
  }
  object PairMyArrayMatcher {
    def unapply[A, B](p: Rep[MyArray[(A, B)]]) = unmkPairMyArray(p)
  }
  lazy val PairMyArray: Rep[PairMyArrayCompanionAbs] = new PairMyArrayCompanionAbs
  implicit def proxyPairMyArrayCompanion(p: Rep[PairMyArrayCompanionAbs]): PairMyArrayCompanionAbs = {
    proxyOps[PairMyArrayCompanionAbs](p)
  }

  implicit case object PairMyArrayCompanionElem extends CompanionElem[PairMyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[PairMyArrayCompanionAbs]
    protected def getDefaultRep = PairMyArray
  }

  implicit def proxyPairMyArray[A, B](p: Rep[PairMyArray[A, B]]): PairMyArray[A, B] =
    proxyOps[PairMyArray[A, B]](p)

  implicit class ExtendedPairMyArray[A, B](p: Rep[PairMyArray[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[PairMyArrayData[A, B]] = isoPairMyArray(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairMyArray[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairMyArrayData[A, B], PairMyArray[A, B]] =
    cachedIso[PairMyArrayIso[A, B]](eA, eB)

  // 6) smart constructor and deconstructor
  def mkPairMyArray[A, B](values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]]
  def unmkPairMyArray[A, B](p: Rep[MyArray[(A, B)]]): Option[(Rep[Collection[(A, B)]])]

  registerModule(MyArrays_Module)
}

// Seq -----------------------------------
trait MyArraysSeq extends ScalanSeq with MyArraysDsl {
  self: ExampleDslSeq =>
  lazy val MyArray: Rep[MyArrayCompanionAbs] = new MyArrayCompanionAbs {
  }

  case class SeqBaseMyArray[A]
      (override val values: Rep[Collection[A]])(implicit eA: Elem[A])
    extends AbsBaseMyArray[A](values) {
  }

  def mkBaseMyArray[A]
    (values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]] =
    new SeqBaseMyArray[A](values)
  def unmkBaseMyArray[A](p: Rep[MyArray[A]]) = p match {
    case p: BaseMyArray[A] @unchecked =>
      Some((p.values))
    case _ => None
  }

  case class SeqPairMyArray[A, B]
      (override val values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsPairMyArray[A, B](values) {
  }

  def mkPairMyArray[A, B]
    (values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
    new SeqPairMyArray[A, B](values)
  def unmkPairMyArray[A, B](p: Rep[MyArray[(A, B)]]) = p match {
    case p: PairMyArray[A, B] @unchecked =>
      Some((p.values))
    case _ => None
  }
}

// Exp -----------------------------------
trait MyArraysExp extends ScalanExp with MyArraysDsl {
  self: ExampleDslExp =>
  lazy val MyArray: Rep[MyArrayCompanionAbs] = new MyArrayCompanionAbs {
  }

  case class ExpBaseMyArray[A]
      (override val values: Rep[Collection[A]])(implicit eA: Elem[A])
    extends AbsBaseMyArray[A](values)

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
    // WARNING: Cannot generate matcher for method `defaultOf`: Method's return type Default[Rep[MyArray[A]]] is not a Rep
  }

  def mkBaseMyArray[A]
    (values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]] =
    new ExpBaseMyArray[A](values)
  def unmkBaseMyArray[A](p: Rep[MyArray[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: BaseMyArrayElem[A] @unchecked =>
      Some((p.asRep[BaseMyArray[A]].values))
    case _ =>
      None
  }

  case class ExpPairMyArray[A, B]
      (override val values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsPairMyArray[A, B](values)

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
        case MethodCall(receiver, method, Seq(ea, eb, _*), _) if receiver.elem == PairMyArrayCompanionElem && method.getName == "defaultOf" =>
          Some((ea, eb)).asInstanceOf[Option[(Elem[A], Elem[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Elem[A], Elem[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `apply`: Method's return type PairMyArray[A, B] is not a Rep
  }

  def mkPairMyArray[A, B]
    (values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
    new ExpPairMyArray[A, B](values)
  def unmkPairMyArray[A, B](p: Rep[MyArray[(A, B)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairMyArrayElem[A, B] @unchecked =>
      Some((p.asRep[PairMyArray[A, B]].values))
    case _ =>
      None
  }

  object MyArrayMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[MyArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "length" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "values" =>
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
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "apply" =>
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
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "mapBy" =>
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
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "zip" =>
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
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == MyArrayCompanionElem && method.getName == "apply" =>
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
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == MyArrayCompanionElem && method.getName == "fromArray" =>
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
        case MethodCall(receiver, method, Seq(len, v, _*), _) if receiver.elem == MyArrayCompanionElem && method.getName == "replicate" =>
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

object MyArrays_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTYgjRRR+3ZlMJskwM8q6uGB0HKKDiyazi7CHAZckm/GHzA/Tc5C4LFQ6lWyv1T/TVRm6PQziSfQmXkX2vjcvguBNEA+eRAXPntb1sKwuHhSrqn/SnZ/eUdg+FF1Vr9979X3fe9V37kGeuvAi1RFBVs3EDNU0+d6grKq1LWYwf9fujwi+hgcfnP9S37WaVIXVLizeRPQaJV0oBi9tz4nfNXzcgSKydEyZ7VIGz3dkhLpuE4J1ZthW3TDNEUM9gusdg7LtDiz07L5/DKegdGBNty3dxQxrLYIoxTRcX8IiIyOeF+Xc33fGMay6OEU9cYojFxmMp89jrAX2h9jRfMu2fJPBSpjaviPS4jYFw3Rsl0UhCtzdTbsfTRcsxBfgyc4tdILqPMSwrjHXsIb8y7KD9HfREO9xE2G+wBOmmAyOfEfOcx0oUXzMAXrTdIhc8RwA4AxclknUxvjUYnxqAp+qhl0DEeM9JDYPXNvzIXiUHIDncBcvP8JF5AG3rX71o+v6Ow+1sqmKjz2RSkGecJE7em6OGiQVHMdvDz+h91+/fUWFUhdKBm30KHORzpKUh2iVkWXZTOYcA4jcIWdrYx5bMkqD20xIoqjbpoMs7imEcpnzRAzdYMJYrC2H7MyBvsAcHJkqnqPE512fc16pmxYi5ODuhVde+K39tgpqOkSRu9S48N3IKYPCrt9wXeSH3sW4ykBpSIjFUPTGYyEjeozD5t3f+99swXU1Ri8MdjbCuIs8/fnH8g8vXVVhqSvlvUPQsMsBpG2CzX23ZVusC0v2CXaDncIJIuJtJoGFPh6gEWEhrEk8chwPButzC9HBAqxtKXolAqAc6HbPtnB156D6p/bdp3eELF1YDnaCyvzHuPL3LysDJhXLYJGnOMI0AjjHSzoNeakVF8KZuBgzUgrCaraJn9i4b9y4/TGT2Cteuu73e7e4/2353TMZNET954/ulvrgwk+fq1DkaPcMZiKnunXGqnmMlQBpfFZaYe+V8rmU3iw3EcWhxGdLOh4qnKVzCfNWMuvKuOU8lYjwtBIJQxoxUHEjCr0gxPpIKqdTrMR1UplHkITj/GHnHLl39WsV8m9BfsDlTzuQ79kjqx/hzG8lhj3WjNaUNM4cV+QiM8ZVPuswPu9ExtKwrKTPNNFAMmTl4KORQ/CrX/1148P333CkRqc60kzA4mkzsxSm2IEUO2J49r8W4M7/TyiZ1qYcL2ZK9/KEdA+Q4U5LN9pemyXTRCYTwk44e9zCnuegmeVgGshpBCqJb17LUGuGwXSYhM9JenK8CWaKfX5ZT/KQIYKy6HQ7yDSIf2l2tJne0/zOVENwaift6KwgTsY7HduEhksRGAxWw5rHHuKtPbzmNnkz2JjTDLSww3OETx9+tnfx+y9+lX8rJXFX8JvViv9nxw1rsjmU2kE0/n+ayJjLS1wfMtt/AVHb0VYvDAAA"
}
}

