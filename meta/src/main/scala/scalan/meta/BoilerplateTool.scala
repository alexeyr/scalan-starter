package scalan.meta

object StarterBoilerplateTool extends BoilerplateTool {
  val starterTypeSynonyms = Map(
    "MyArr" -> "MyArray"
    // declare your type synonims for User Defined types here (see type PA[A] = Rep[PArray[A]])
  )
  lazy val starterConfig = CodegenConfig(
    name = "ml",
    srcPath = "src/main/scala",
    entityFiles = List(
      "scalan/examples/MyArrays.scala"
    ),
    entityTypeSynonyms = starterTypeSynonyms,
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    isAlreadyRep = true
  )

  override def getConfigs(args: Array[String]) = Seq(starterConfig)

  override def main(args: Array[String]) = super.main(args)
}
