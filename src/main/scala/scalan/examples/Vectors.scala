package scalan.examples

/**
 * Created by Victor Smirnov on 3/12/15.
 */

import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance

trait Vectors { self: VectorsDsl with ScalanDsl =>

  type RVector[T] = Rep[Vector[T]]

  trait Vector[T] extends Reifiable[Vector[T]] {
    implicit def eT: Elem[T]

    def length: Rep[Int]
    def items: Rep[Array[T]]
    def *^(other: RVector[T])(implicit n: Numeric[T]): RVector[T]
    def dot(other: RVector[T])(implicit n: Numeric[T]): Rep[T]
    def sum(implicit n: Numeric[T]): Rep[T]
  }

  abstract class DenseVector[T](val items: Rep[Array[T]])
                               (implicit val eT: Elem[T])
    extends Vector[T] {

    def length = items.length

    def *^(other: RVector[T])(implicit n: Numeric[T]): RVector[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices, nonZeroValues, _) =>
          val nonZeroValuesNew = (nonZeroValues zip items(nonZeroIndices)).map { case Pair(v1, v2) => v1 * v2 }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
        case _ =>
          DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 * v2 })
      }
    }

    def dot(other: RVector[T])(implicit n: Numeric[T]): Rep[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices, nonZeroValues, _) =>
          (items(nonZeroIndices) zip nonZeroValues).map { case Pair(v1, v2) => v1 * v2 }.reduce
        case _ =>
          (other.items zip items).map { case Pair(v1, v2) => v1 * v2 }.reduce
      }
    }

    def sum(implicit n: Numeric[T]): Rep[T] = items.reduce
  }

  abstract class ConstVector[T](val item: Rep[T], val length: Rep[Int])
                               (implicit val eT: Elem[T])
    extends Vector[T] {

    def items: Rep[Array[T]] = SArray.replicate(length, item)

    def *^(other: RVector[T])(implicit n: Numeric[T]): RVector[T] = {
      other match {
        case ConstVectorMatcher(otherItem, _) =>
          ConstVector(item * otherItem, length)
        case SparseVectorMatcher(nonZeroIndices, nonZeroValues, _) =>
          val nonZeroValuesNew = nonZeroValues.map { v => v * item }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
        case _ =>
          DenseVector(other.items.map { v => v * item })
      }
    }

    def dot(other: RVector[T])(implicit n: Numeric[T]): Rep[T] = {
      other.sum * item
    }

    def sum(implicit n: Numeric[T]): Rep[T] = item * length.asRep[T]
  }

  abstract class SparseVector[T](val nonZeroIndices: Rep[Array[Int]],
                                 val nonZeroValues: Rep[Array[T]],
                                 val length: Rep[Int])(implicit val eT: Elem[T])
    extends Vector[T] {

    lazy val zeroValue = eT.defaultRepValue

    def items: Rep[Array[T]] = SArray.replicate(length, zeroValue).updateMany(nonZeroIndices, nonZeroValues)

    def *^(other: RVector[T])(implicit n: Numeric[T]): RVector[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices1, nonZeroValues1, _) =>
          ???
          //val newItems = (nonZeroIndices zip nonZeroValues) innerMult (nonZeroIndices1 zip nonZeroValues1)
          //SparseVector(newItems.as, newItems.bs, length)
        case _ =>
          other *^ self
      }
    }

    def sum(implicit n: Numeric[T]): Rep[T] = nonZeroValues.reduce

    def dot(other: Rep[Vector[T]])(implicit n: Numeric[T]): Rep[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices1, nonZeroValues1, _) =>
          dotSparse(nonZeroIndices, nonZeroValues, nonZeroIndices1, nonZeroValues1)
        case _ =>
          other.dot(self)
      }
    }
  }

  trait VectorCompanion
}

trait VectorsDsl extends impl.VectorsAbs { self: ScalanDsl =>
  def dotSparse[T: Elem](xIndices: Rep[Array[Int]], xValues: Rep[Array[T]], yIndices: Rep[Array[Int]], yValues: Rep[Array[T]])
                        (implicit n: Numeric[T]): Rep[T]
}

trait VectorsDslSeq extends impl.VectorsSeq { self: ScalanCtxSeq =>

  def dotSparse[T: Elem](xIndices: Rep[Array[Int]], xValues: Rep[Array[T]], yIndices: Rep[Array[Int]], yValues: Rep[Array[T]])
                        (implicit n: Numeric[T]): Rep[T] = {
    var result = n.zero
    val yMap = yIndices.zip(yValues).toMap
    xIndices.zip(xValues).foldLeft(n.zero) {
      case (acc, (i, x)) =>
      yMap.get(i) match {
        case Some(y) => acc + x * y
        case None => acc
      }
    }
  }

}

trait VectorsDslExp extends impl.VectorsExp { self: ScalanCtxExp =>
  def dotSparse[T: Elem](xIndices: Rep[Array[Int]], xValues: Rep[Array[T]], yIndices: Rep[Array[Int]], yValues: Rep[Array[T]])
                        (implicit n: Numeric[T]): Rep[T] = {
    DotSparse(xIndices, xValues, yIndices, yValues)
  }

  case class DotSparse[T](xIndices: Arr[Int], xValues: Arr[T], yIndices: Arr[Int], yValues: Arr[T])
                         (implicit val n: Numeric[T], selfType: Elem[T]) extends BaseDef[T] {
    override def mirror(f: Transformer) = DotSparse(f(xIndices), f(xValues), f(yIndices), f(yValues))
  }

}
