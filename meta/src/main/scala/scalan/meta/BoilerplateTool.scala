package scalan.meta

object StarterBoilerplateTool extends BoilerplateTool {
  val starterTypeSynonyms = Map(
    "RVector" -> "Vector",
    "RMatrix" -> "Matrix"
  )
  lazy val starterConfig = CodegenConfig(
    name = "starter",
    srcPath = "src/main/scala",
    entityFiles = List(
      "scalan/examples/Vectors.scala",
      "scalan/examples/Matrices.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    entityTypeSynonyms = starterTypeSynonyms
  )

  override def getConfigs(args: Array[String]) = Seq(starterConfig)

  override def main(args: Array[String]) = super.main(args)
}
