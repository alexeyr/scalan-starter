package scalan.examples

import scalan.{ScalanDsl, ScalanCtxSeq, ScalanCtxExp}

/**
 * Created by Viktor Smirnov on 29.03.15.
 */
trait ExampleDsl extends ScalanDsl with MatricesDsl

trait ExampleDslSeq extends ExampleDsl with MatricesDslSeq with ScalanCtxSeq

trait ExampleDslExp extends ExampleDsl with MatricesDslExp with ScalanCtxExp
