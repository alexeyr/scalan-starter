package scalan.examples

/**
  * Created by Victor Smirnov on 3/12/15.
  */

import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance

trait Matrices extends Vectors { self: MatricesDsl with ScalanDsl =>

  type RMatrix[T] = Rep[Matrix[T]]

  trait Matrix[T] extends Reifiable[Matrix[T]] {
    implicit def eT: Elem[T]

    def numColumns: Rep[Int]
    def numRows: Rep[Int]
    def rows: Rep[Array[Vector[T]]]
    def columns: Rep[Array[Vector[T]]]
  }

  abstract class FlatMatrix[T](val rmValues: Rep[Array[T]], val numColumns: Rep[Int])
                              (implicit val eT: Elem[T]) extends Matrix[T] {
    def items = rmValues
    def numRows: Rep[Int] = rmValues.length /! numColumns
    def columns: Rep[Array[Vector[T]]] = {
      SArray.rangeFrom0(numColumns).map { i =>
        DenseVector(rmValues.stride(i, numRows, numColumns))
      }
    }
    def rows: Rep[Array[Vector[T]]] =
      rmValues.grouped(numColumns).map { row => DenseVector(row) }
  }

  abstract class CompoundMatrix[T](val rows: Rep[Array[Vector[T]]], val numColumns: Rep[Int])
                                  (implicit val eT: Elem[T]) extends Matrix[T] {
    def columns: Rep[Array[Vector[T]]] = {
      SArray.tabulate(numColumns) { j => DenseVector(rows.map(_.items(j))) }
    }
    def numRows = rows.length
  }

  abstract class ConstMatrix[T](val item: Rep[T], val numColumns: Rep[Int], val numRows: Rep[Int])
                                   (implicit val eT: Elem[T]) extends Matrix[T] {
    def items = SArray.replicate(numColumns * numRows, item)
    def columns: Rep[Array[Vector[T]]] = {
      SArray.rangeFrom0(numColumns).map { i => ConstVector(item, numRows) }
    }
    def rows: Rep[Array[Vector[T]]] = {
      SArray.rangeFrom0(numRows).map { i => ConstVector(item, numColumns) }
    }
  }

  trait MatrixCompanion
}

trait MatricesDsl extends impl.MatricesAbs with VectorsDsl { self: ScalanDsl =>
}

trait MatricesDslSeq extends impl.MatricesSeq with VectorsDslSeq { self: ScalanCtxSeq => }

trait MatricesDslExp extends impl.MatricesExp with VectorsDslExp { self: ScalanCtxExp => }
