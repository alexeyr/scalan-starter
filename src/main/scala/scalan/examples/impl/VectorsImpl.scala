package scalan.examples

import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe._
import scalan.common.Default
import scalan.meta.ScalanAst._

package impl {

// Abs -----------------------------------
trait VectorsAbs extends Vectors with Scalan {
  self: VectorsDsl with ScalanDsl =>

  // single proxy for each type family
  implicit def proxyVector[T](p: Rep[Vector[T]]): Vector[T] = {
    proxyOps[Vector[T]](p)(scala.reflect.classTag[Vector[T]])
  }

  // familyElem
  class VectorElem[T, To <: Vector[T]](implicit val eT: Elem[T])
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Vectors")
      module.entities.find(_.name == "Vector").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[Vector[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Vector[T]] => convertVector(x) }
      tryConvert(element[Vector[T]], this, x, conv)
    }

    def convertVector(x : Rep[Vector[T]]): Rep[To] = {
      assert(x.selfType1 match { case _: VectorElem[_, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def vectorElement[T](implicit eT: Elem[T]): Elem[Vector[T]] =
    new VectorElem[T, Vector[T]]

  implicit case object VectorCompanionElem extends CompanionElem[VectorCompanionAbs] {
    lazy val tag = weakTypeTag[VectorCompanionAbs]
    protected def getDefaultRep = Vector
  }

  abstract class VectorCompanionAbs extends CompanionBase[VectorCompanionAbs] with VectorCompanion {
    override def toString = "Vector"
  }
  def Vector: Rep[VectorCompanionAbs]
  implicit def proxyVectorCompanion(p: Rep[VectorCompanion]): VectorCompanion =
    proxyOps[VectorCompanion](p)

  // elem for concrete class
  class DenseVectorElem[T](val iso: Iso[DenseVectorData[T], DenseVector[T]])(implicit eT: Elem[T])
    extends VectorElem[T, DenseVector[T]]
    with ConcreteElem[DenseVectorData[T], DenseVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Vectors")
      module.concreteSClasses.find(_.name == "DenseVector").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = DenseVector(x.items)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[DenseVector[T]]
    }
  }

  // state representation type
  type DenseVectorData[T] = Array[T]

  // 3) Iso for concrete class
  class DenseVectorIso[T](implicit eT: Elem[T])
    extends Iso[DenseVectorData[T], DenseVector[T]] {
    override def from(p: Rep[DenseVector[T]]) =
      p.items
    override def to(p: Rep[Array[T]]) = {
      val items = p
      DenseVector(items)
    }
    lazy val defaultRepTo: Rep[DenseVector[T]] = DenseVector(element[Array[T]].defaultRepValue)
    lazy val eTo = new DenseVectorElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class DenseVectorCompanionAbs extends CompanionBase[DenseVectorCompanionAbs] {
    override def toString = "DenseVector"

    def apply[T](items: Rep[Array[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
      mkDenseVector(items)
  }
  object DenseVectorMatcher {
    def unapply[T](p: Rep[Vector[T]]) = unmkDenseVector(p)
  }
  def DenseVector: Rep[DenseVectorCompanionAbs]
  implicit def proxyDenseVectorCompanion(p: Rep[DenseVectorCompanionAbs]): DenseVectorCompanionAbs = {
    proxyOps[DenseVectorCompanionAbs](p)
  }

  implicit case object DenseVectorCompanionElem extends CompanionElem[DenseVectorCompanionAbs] {
    lazy val tag = weakTypeTag[DenseVectorCompanionAbs]
    protected def getDefaultRep = DenseVector
  }

  implicit def proxyDenseVector[T](p: Rep[DenseVector[T]]): DenseVector[T] =
    proxyOps[DenseVector[T]](p)

  implicit class ExtendedDenseVector[T](p: Rep[DenseVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[DenseVectorData[T]] = isoDenseVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoDenseVector[T](implicit eT: Elem[T]): Iso[DenseVectorData[T], DenseVector[T]] =
    new DenseVectorIso[T]

  // 6) smart constructor and deconstructor
  def mkDenseVector[T](items: Rep[Array[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]]
  def unmkDenseVector[T](p: Rep[Vector[T]]): Option[(Rep[Array[T]])]

  // elem for concrete class
  class ConstVectorElem[T](val iso: Iso[ConstVectorData[T], ConstVector[T]])(implicit eT: Elem[T])
    extends VectorElem[T, ConstVector[T]]
    with ConcreteElem[ConstVectorData[T], ConstVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Vectors")
      module.concreteSClasses.find(_.name == "ConstVector").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = // Converter is not generated by meta
!!!("Cannot convert from Vector to ConstVector: missing fields List(item)")
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ConstVector[T]]
    }
  }

  // state representation type
  type ConstVectorData[T] = (T, Int)

  // 3) Iso for concrete class
  class ConstVectorIso[T](implicit eT: Elem[T])
    extends Iso[ConstVectorData[T], ConstVector[T]]()(pairElement(implicitly[Elem[T]], implicitly[Elem[Int]])) {
    override def from(p: Rep[ConstVector[T]]) =
      (p.item, p.length)
    override def to(p: Rep[(T, Int)]) = {
      val Pair(item, length) = p
      ConstVector(item, length)
    }
    lazy val defaultRepTo: Rep[ConstVector[T]] = ConstVector(element[T].defaultRepValue, 0)
    lazy val eTo = new ConstVectorElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class ConstVectorCompanionAbs extends CompanionBase[ConstVectorCompanionAbs] {
    override def toString = "ConstVector"
    def apply[T](p: Rep[ConstVectorData[T]])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      isoConstVector(eT).to(p)
    def apply[T](item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      mkConstVector(item, length)
  }
  object ConstVectorMatcher {
    def unapply[T](p: Rep[Vector[T]]) = unmkConstVector(p)
  }
  def ConstVector: Rep[ConstVectorCompanionAbs]
  implicit def proxyConstVectorCompanion(p: Rep[ConstVectorCompanionAbs]): ConstVectorCompanionAbs = {
    proxyOps[ConstVectorCompanionAbs](p)
  }

  implicit case object ConstVectorCompanionElem extends CompanionElem[ConstVectorCompanionAbs] {
    lazy val tag = weakTypeTag[ConstVectorCompanionAbs]
    protected def getDefaultRep = ConstVector
  }

  implicit def proxyConstVector[T](p: Rep[ConstVector[T]]): ConstVector[T] =
    proxyOps[ConstVector[T]](p)

  implicit class ExtendedConstVector[T](p: Rep[ConstVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[ConstVectorData[T]] = isoConstVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoConstVector[T](implicit eT: Elem[T]): Iso[ConstVectorData[T], ConstVector[T]] =
    new ConstVectorIso[T]

  // 6) smart constructor and deconstructor
  def mkConstVector[T](item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]]
  def unmkConstVector[T](p: Rep[Vector[T]]): Option[(Rep[T], Rep[Int])]

  // elem for concrete class
  class SparseVectorElem[T](val iso: Iso[SparseVectorData[T], SparseVector[T]])(implicit eT: Elem[T])
    extends VectorElem[T, SparseVector[T]]
    with ConcreteElem[SparseVectorData[T], SparseVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("Vectors")
      module.concreteSClasses.find(_.name == "SparseVector").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = // Converter is not generated by meta
!!!("Cannot convert from Vector to SparseVector: missing fields List(nonZeroIndices, nonZeroValues)")
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SparseVector[T]]
    }
  }

  // state representation type
  type SparseVectorData[T] = (Array[Int], (Array[T], Int))

  // 3) Iso for concrete class
  class SparseVectorIso[T](implicit eT: Elem[T])
    extends Iso[SparseVectorData[T], SparseVector[T]]()(pairElement(implicitly[Elem[Array[Int]]], pairElement(implicitly[Elem[Array[T]]], implicitly[Elem[Int]]))) {
    override def from(p: Rep[SparseVector[T]]) =
      (p.nonZeroIndices, p.nonZeroValues, p.length)
    override def to(p: Rep[(Array[Int], (Array[T], Int))]) = {
      val Pair(nonZeroIndices, Pair(nonZeroValues, length)) = p
      SparseVector(nonZeroIndices, nonZeroValues, length)
    }
    lazy val defaultRepTo: Rep[SparseVector[T]] = SparseVector(element[Array[Int]].defaultRepValue, element[Array[T]].defaultRepValue, 0)
    lazy val eTo = new SparseVectorElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class SparseVectorCompanionAbs extends CompanionBase[SparseVectorCompanionAbs] {
    override def toString = "SparseVector"
    def apply[T](p: Rep[SparseVectorData[T]])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      isoSparseVector(eT).to(p)
    def apply[T](nonZeroIndices: Rep[Array[Int]], nonZeroValues: Rep[Array[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      mkSparseVector(nonZeroIndices, nonZeroValues, length)
  }
  object SparseVectorMatcher {
    def unapply[T](p: Rep[Vector[T]]) = unmkSparseVector(p)
  }
  def SparseVector: Rep[SparseVectorCompanionAbs]
  implicit def proxySparseVectorCompanion(p: Rep[SparseVectorCompanionAbs]): SparseVectorCompanionAbs = {
    proxyOps[SparseVectorCompanionAbs](p)
  }

  implicit case object SparseVectorCompanionElem extends CompanionElem[SparseVectorCompanionAbs] {
    lazy val tag = weakTypeTag[SparseVectorCompanionAbs]
    protected def getDefaultRep = SparseVector
  }

  implicit def proxySparseVector[T](p: Rep[SparseVector[T]]): SparseVector[T] =
    proxyOps[SparseVector[T]](p)

  implicit class ExtendedSparseVector[T](p: Rep[SparseVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[SparseVectorData[T]] = isoSparseVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSparseVector[T](implicit eT: Elem[T]): Iso[SparseVectorData[T], SparseVector[T]] =
    new SparseVectorIso[T]

  // 6) smart constructor and deconstructor
  def mkSparseVector[T](nonZeroIndices: Rep[Array[Int]], nonZeroValues: Rep[Array[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]]
  def unmkSparseVector[T](p: Rep[Vector[T]]): Option[(Rep[Array[Int]], Rep[Array[T]], Rep[Int])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Vectors_Module.dump))
}

// Seq -----------------------------------
trait VectorsSeq extends VectorsDsl with ScalanSeq {
  self: VectorsDsl with ScalanCtxSeq =>
  lazy val Vector: Rep[VectorCompanionAbs] = new VectorCompanionAbs with UserTypeSeq[VectorCompanionAbs] {
    lazy val selfType = element[VectorCompanionAbs]
  }

  case class SeqDenseVector[T]
      (override val items: Rep[Array[T]])
      (implicit eT: Elem[T])
    extends DenseVector[T](items)
        with UserTypeSeq[DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]]
  }
  lazy val DenseVector = new DenseVectorCompanionAbs with UserTypeSeq[DenseVectorCompanionAbs] {
    lazy val selfType = element[DenseVectorCompanionAbs]
  }

  def mkDenseVector[T]
      (items: Rep[Array[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
      new SeqDenseVector[T](items)
  def unmkDenseVector[T](p: Rep[Vector[T]]) = p match {
    case p: DenseVector[T] @unchecked =>
      Some((p.items))
    case _ => None
  }

  case class SeqConstVector[T]
      (override val item: Rep[T], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends ConstVector[T](item, length)
        with UserTypeSeq[ConstVector[T]] {
    lazy val selfType = element[ConstVector[T]]
  }
  lazy val ConstVector = new ConstVectorCompanionAbs with UserTypeSeq[ConstVectorCompanionAbs] {
    lazy val selfType = element[ConstVectorCompanionAbs]
  }

  def mkConstVector[T]
      (item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      new SeqConstVector[T](item, length)
  def unmkConstVector[T](p: Rep[Vector[T]]) = p match {
    case p: ConstVector[T] @unchecked =>
      Some((p.item, p.length))
    case _ => None
  }

  case class SeqSparseVector[T]
      (override val nonZeroIndices: Rep[Array[Int]], override val nonZeroValues: Rep[Array[T]], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length)
        with UserTypeSeq[SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]]
  }
  lazy val SparseVector = new SparseVectorCompanionAbs with UserTypeSeq[SparseVectorCompanionAbs] {
    lazy val selfType = element[SparseVectorCompanionAbs]
  }

  def mkSparseVector[T]
      (nonZeroIndices: Rep[Array[Int]], nonZeroValues: Rep[Array[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      new SeqSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T](p: Rep[Vector[T]]) = p match {
    case p: SparseVector[T] @unchecked =>
      Some((p.nonZeroIndices, p.nonZeroValues, p.length))
    case _ => None
  }
}

// Exp -----------------------------------
trait VectorsExp extends VectorsDsl with ScalanExp {
  self: VectorsDsl with ScalanCtxExp =>
  lazy val Vector: Rep[VectorCompanionAbs] = new VectorCompanionAbs with UserTypeDef[VectorCompanionAbs] {
    lazy val selfType = element[VectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpDenseVector[T]
      (override val items: Rep[Array[T]])
      (implicit eT: Elem[T])
    extends DenseVector[T](items) with UserTypeDef[DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]]
    override def mirror(t: Transformer) = ExpDenseVector[T](t(items))
  }

  lazy val DenseVector: Rep[DenseVectorCompanionAbs] = new DenseVectorCompanionAbs with UserTypeDef[DenseVectorCompanionAbs] {
    lazy val selfType = element[DenseVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object DenseVectorMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], RVector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$times$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], RVector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], RVector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], RVector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[DenseVector[T]], RVector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], RVector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkDenseVector[T]
    (items: Rep[Array[T]])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
    new ExpDenseVector[T](items)
  def unmkDenseVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: DenseVectorElem[T] @unchecked =>
      Some((p.asRep[DenseVector[T]].items))
    case _ =>
      None
  }

  case class ExpConstVector[T]
      (override val item: Rep[T], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends ConstVector[T](item, length) with UserTypeDef[ConstVector[T]] {
    lazy val selfType = element[ConstVector[T]]
    override def mirror(t: Transformer) = ExpConstVector[T](t(item), t(length))
  }

  lazy val ConstVector: Rep[ConstVectorCompanionAbs] = new ConstVectorCompanionAbs with UserTypeDef[ConstVectorCompanionAbs] {
    lazy val selfType = element[ConstVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object ConstVectorMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], RVector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$times$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], RVector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], RVector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], RVector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[ConstVector[T]], RVector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], RVector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkConstVector[T]
    (item: Rep[T], length: Rep[Int])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
    new ExpConstVector[T](item, length)
  def unmkConstVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConstVectorElem[T] @unchecked =>
      Some((p.asRep[ConstVector[T]].item, p.asRep[ConstVector[T]].length))
    case _ =>
      None
  }

  case class ExpSparseVector[T]
      (override val nonZeroIndices: Rep[Array[Int]], override val nonZeroValues: Rep[Array[T]], override val length: Rep[Int])
      (implicit eT: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length) with UserTypeDef[SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]]
    override def mirror(t: Transformer) = ExpSparseVector[T](t(nonZeroIndices), t(nonZeroValues), t(length))
  }

  lazy val SparseVector: Rep[SparseVectorCompanionAbs] = new SparseVectorCompanionAbs with UserTypeDef[SparseVectorCompanionAbs] {
    lazy val selfType = element[SparseVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SparseVectorMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[SparseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], RVector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$times$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], RVector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], RVector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[SparseVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[Vector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[Vector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[Vector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkSparseVector[T]
    (nonZeroIndices: Rep[Array[Int]], nonZeroValues: Rep[Array[T]], length: Rep[Int])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
    new ExpSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SparseVectorElem[T] @unchecked =>
      Some((p.asRep[SparseVector[T]].nonZeroIndices, p.asRep[SparseVector[T]].nonZeroValues, p.asRep[SparseVector[T]].length))
    case _ =>
      None
  }

  object VectorMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object items {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], RVector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$times$up" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], RVector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], RVector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], RVector[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(other, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "dot" =>
          Some((receiver, other, n)).asInstanceOf[Option[(Rep[Vector[T]], RVector[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], RVector[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object VectorCompanionMethods {
  }
}

object Vectors_Module {
  val packageName = "scalan.examples"
  val name = "Vectors"
  val dump = "H4sIAAAAAAAAAM1WTWwbRRR+u/FPbIekVFBRiYoQXBAI7KgS6iGHKk0cFHCTKBsqZCqk8XrsTNmd3eyMozWHHjjxcwOuCPXeGxckpF4QEuLACQESZ06lFaqgPYF4M7vrtWO7NUVF+DDyzr59P9/3vTdz7SZkRQDPCps4hFdcKknF0v9XhSxbNS6Z7F3wWl2HrtP2uye+sC/w88KEhQbk9olYF04DCtGfWuj3/1v0oA4Fwm0qpBcICU/XdYSq7TkOtSXzeJW5bleSpkOrdSbkSh0yTa/VO4ArYNThmO1xO6CSWmsOEYKKeH+WqoxY/7mgn3vbfhqDV1UV1YEq9gLCJKaPMY5F9rvUt3rc4z1Xwnyc2rav0kKbPHN9L5BJiDy62/dayWOGE9yA4/XL5JBUMUSnasmA8Q5+WfKJ/Tbp0C00UeYZTFhQp73X8/XzTB2Kgh4gQJuu7+id0AcAZOCMTqKS4lPp41NR+JQtGjDisHeIerkTeGEPop8xAxD66OLF+7hIPNAab5U/uGS/edcquab6OFSp5HWFOXT01AQ1aCoQx292PxK3X7l61oRiA4pMrDaFDIgtBymP0SoRzj2pc+4DSIIOsrU0iS0dZRVtjkiiYHuuTzh6iqGcQ54cZjOpjNXeXMzOBOjz0qeJqRH6Rr/exQn1at2sEcfZuXHypdO/1t4wwRwOUUCXFgo/SJxKyF1E8GMAcnpdkGDsaYTVUgjTNX+P4H0Ynrtxq/X1Mlwy++DFsabjC11kxU8/lL5//pwJsw2t7g2HdBqIn6g51N0O1jwuGzDrHdIgepM/JI76N5a/fIu2SdeRMaqDcMwgHBIWJ/ahTxVWK1rzRgJAKZLtlsdpeWOnfMf69pNrSpUBzEVvosb8i5398+f5ttSClZBlkroiwXcGG3oY8exqEJDeVCzEXKjllDZ9fOCzJ4wkVf1egkn3En8ZBd99Q0gorVMuaKSLNIpi7tQk5WmlntitP+bcPHfdhOyrkG0jIaIO2abX5a2kBXBMShrK88meMUwISp4ExE0UEw2MRdBJ6ESPZKwNS8ZwTf9M0SMAwhEAM5q5icSN5jPiIedQ3pH7Y3wE8MxkRHcC5uLZcUhf/urL13+7vpXV7Xw8lvNF4nRpNMlj8FIgleCMZYy0yeV/qRtsTCFHdaPWM/8LLufxDG3QwNvkLYaDf7p2VMtrg8HG+34k9q2J+fedPi6IWjZGPY8k+JB5nrOwS8cNiOmIHqjvXqdBcgv6o7Fs/n7yx89MKODQbzLpEr+8POXZ/RDPYxhGqrBLWZupy8uDK3iAOXXSFKPzxPJc+ujSbfbW1Q+lPlSNcPg+t928jDFW9MdP6hDvQeooJRunwkKUzdogFBFJ6VXgQRlV6/upTWyYj+vH2DHRNCQIfNwiH2OdSxMUYMX4owiu3P1064XvPv9Fj8CiYhKPX96/8w6OvmGki3F4vMOmM0LRFcVJthNpZxTnuoa/AaFIgr9qDAAA"
}
}

