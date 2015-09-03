package scalan.examples

import scalan._
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.uni._
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.{LmsCompilerScala, CommunityLmsCompilerScala}
import scalan.it.BaseItTests
import scalan.util.FileUtil._

trait LinearAlgebraExamples extends MatricesDsl { self: ScalanDsl =>
  def mvm[T](matrix: Rep[Matrix[T]], vector: Rep[Vector[T]])(implicit eT: Elem[T], n: Numeric[T]): Rep[Vector[T]] =
    DenseVector(matrix.rows.map(r => r dot vector))

  lazy val abstractMvm = fun { p: Rep[(Matrix[Double], Vector[Double])] =>
    val Pair(matrix, vector) = p
    mvm(matrix, vector)
  }

  lazy val ddmvm = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix = CompoundMatrix(m.map { r: Arr[Double] => DenseVector(r): RVector[Double] }, width)
    val vector = DenseVector(v)
    mvm(matrix, vector).items
  }

  lazy val dsmvm = fun { p: Rep[(Array[Array[Double]], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0).length
    val matrix = CompoundMatrix(m.map { r: Arr[Double] => DenseVector(r): RVector[Double] }, width)
    val vector = SparseVector(vIs, vVs, vL)
    mvm(matrix, vector).items
  }

  lazy val sdmvm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0)._3
      val matrix = CompoundMatrix(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
        SparseVector(r._1, r._2, r._3): RVector[Double] },
      width)
    val vector = DenseVector(v)
    mvm(matrix, vector).items
  }

  lazy val ssmvm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0)._3
    val matrix = CompoundMatrix(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
        SparseVector(r._1, r._2, r._3): RVector[Double] }, width)
    val vector = SparseVector(vIs, vVs, vL)
    mvm(matrix, vector).items
  }

  lazy val fdmvm = fun { p: Rep[((Array[Double], Int), Array[Double])] =>
    val Pair(m, v) = p
    val matrix = FlatMatrix(m._1, m._2)
    val vector = DenseVector(v)
    mvm(matrix, vector).items
  }

  lazy val fsmvm = fun { p: Rep[((Array[Double], Int), (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix = FlatMatrix(m._1, m._2)
    val vector = SparseVector(vIs, vVs, vL)
    mvm(matrix, vector).items
  }

  lazy val denseVector = fun { p: Rep[Array[Double]] =>
    DenseVector(p)
  }
  lazy val constVector = fun { p: Rep[(Double, Int)] =>
    ConstVector(p._1, p._2)
  }

  lazy val flatMatrix = fun { p: Rep[(Array[Double], Int)] =>
    FlatMatrix(p._1, p._2)
  }
  lazy val constMatrix = fun { p: Rep[(Double, (Int, Int))] =>
    ConstMatrix(p._1, p._2, p._3)
  }

}

abstract class LmsLinAlgItTests extends BaseItTests {
  class ProgExp extends LinearAlgebraExamples with ScalanCtxExp with MatricesDslExp with JNIExtractorOpsExp

  class ProgSeq extends LinearAlgebraExamples with ScalanCtxSeq with MatricesDslSeq

  val progStaged = new LmsCompilerScala(new ProgExp) with CoreBridge {
    val lms = new CommunityLmsBackend
  }

  val progSeq = new ProgSeq

  def sparseVectorData(arr: Array[Double]) = (arr.indices.toArray, (arr, arr.length))
}

class LmsMvmItTests extends LmsLinAlgItTests {
  import progSeq._

  test("abstractMvm") {
    val dot = file(prefix, "abstract.dot")
    progStaged.scalan.emitDepGraph(progStaged.scalan.abstractMvm, dot)(GraphVizConfig.default)
  }

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.ddmvm, "ddmvm", in)

    val dir = file(prefix, "ddmvm")
  }

  test("dsmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.dsmvm, "dsmvm", in)
  }

  test("sdmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.sdmvm, "sdmvm", in)
  }

  ignore("ssmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.ssmvm, "ssmvm", in)
  }

  test("fdmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.fdmvm, "fdmvm", in)
  }

  test("fsmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.fsmvm, "fsmvm", in)
  }
}
