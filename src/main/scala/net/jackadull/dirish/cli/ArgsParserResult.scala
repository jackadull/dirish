package net.jackadull.dirish.cli

sealed trait ArgsParserResult[+R]
final case class ArgsParserSuccess[+R](result:R, remainingArgs:List[String]) extends ArgsParserResult[R]

sealed trait ArgsParserFailure extends ArgsParserResult[Nothing] {
  def combineOr(that:ArgsParserFailure):ArgsParserFailure
  def hasPriority:Boolean
  def message:String
  def withPriority:ArgsParserFailure
}
final case class ExpectedParserFailure(expectationAlternatives:Seq[String], insteadOf:List[String], hasPriority:Boolean=false)
extends ArgsParserFailure {
  def message:String = s"Expected $describeExpectation $describeInsteadOf."

  def combineOr(that:ArgsParserFailure):ArgsParserFailure = that match {
    case ExpectedParserFailure(thatAlts, thatIO, _) ⇒ ExpectedParserFailure(
      expectationAlternatives ++ thatAlts,
      if(insteadOf.length<thatIO.length) thatIO else insteadOf,
      hasPriority || that.hasPriority
    )
  }

  def withPriority:ArgsParserFailure = copy(hasPriority = true)

  private def describeExpectation:String = expectationAlternatives match {
    case Nil ⇒ "nothing"
    case a :: Nil ⇒ a
    case a :: b :: Nil ⇒ s"either $a or $b"
    case _ ⇒ s"any of ${expectationAlternatives.mkString("(", ", ", ")")}"
  }
  private def describeInsteadOf:String = insteadOf match {
    case a :: b :: c :: _ :: r ⇒ s"instead of '$a $b $c ...'"
    case Nil ⇒ "at the end of the argument list"
    case _ ⇒ s"instead of '${insteadOf mkString " "}'"
  }
}
