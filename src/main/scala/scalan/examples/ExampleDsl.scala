package scalan.examples

import scalan.{ScalanCommunityDsl, ScalanCommunityDslSeq, ScalanCommunityDslExp}

/**
 * Created by Viktor Smirnov on 29.03.15.
 */
trait ExampleDsl extends ScalanCommunityDsl with MyArraysDsl

trait ExampleDslSeq extends ScalanCommunityDslSeq with ExampleDsl with MyArraysDslSeq

trait ExampleDslExp extends ScalanCommunityDslExp with ExampleDsl with MyArraysDslExp
