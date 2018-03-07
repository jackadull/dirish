package net.jackadull.dirish.cli

import net.jackadull.dirish.cli.ArgsParser.{AndThenArgsParser, MapArgsParser, OptArgsParser, OrArgsParser, PriorityFailureParser}

import scala.language.{implicitConversions, postfixOps}

trait ArgsParser[+R] {
  def apply(args:List[String]):ArgsParserResult[R]

  def ^^[R2](f:R⇒R2):ArgsParser[R2] = map(f)
  def |[R2>:R](that:ArgsParser[R2]):ArgsParser[R2] = this or that
  def |>[R2>:R](that:ArgsParser[R2]):ArgsParser[R2] = this.or(that, combineFailures = false)
  def ~[R2,R3](that:ArgsParser[R2])(f:(R,R2)⇒R3):ArgsParser[R3] = this.andThen(that)(f)
  def ~>[R2](that:ArgsParser[R2]):ArgsParser[R2] = (this ~ that)((_,r) ⇒ r)
  def <~[R2](that:ArgsParser[R2]):ArgsParser[R] = (this ~ that)((r,_) ⇒ r)
  def ? :ArgsParser[Option[R]] = opt
  def ! :ArgsParser[R] = withPriorityFailure

  def andThen[R2,R3](that:ArgsParser[R2])(f:(R,R2)⇒R3):ArgsParser[R3] = AndThenArgsParser(this, that, f)
  def map[R2](f:R⇒R2):ArgsParser[R2] = MapArgsParser(this, f)
  def or[R2>:R](that:ArgsParser[R2], combineFailures:Boolean=true):ArgsParser[R2] = OrArgsParser(this, that, combineFailures)
  def opt:ArgsParser[Option[R]] = OptArgsParser(this)
  def withPriorityFailure:ArgsParser[R] = PriorityFailureParser(this)
}
object ArgsParser {
  implicit def constantStringParser(str:String):ArgsParser[String] = ConstantStringParser(str)
  val endOfArguments:ArgsParser[Unit] = EndOfArgsParser
  def expecting(expectationName:String):ArgsParser[String] = AnyStringParser(expectationName)

  private[cli] final case class AnyStringParser(errorExpectation:String) extends ArgsParser[String] {
    def apply(args:List[String]):ArgsParserResult[String] = args match {
      case str :: rest ⇒ ArgsParserSuccess(str, rest)
      case _ ⇒ ExpectedParserFailure(Seq(errorExpectation), args)
    }
  }
  private[cli] final case class ConstantStringParser(str:String) extends ArgsParser[String] {
    def apply(args:List[String]):ArgsParserResult[String] = args match {
      case `str` :: rest ⇒ ArgsParserSuccess(str, rest)
      case _ ⇒ ExpectedParserFailure(Seq(s"'$str'"), args)
    }
  }
  private[cli] object EndOfArgsParser extends ArgsParser[Unit] {
    def apply(args:List[String]):ArgsParserResult[Unit] = args match {
      case Nil ⇒ ArgsParserSuccess((), Nil)
      case _ ⇒ ExpectedParserFailure(Seq("no further arguments"), args)
    }
  }

  private[cli] final case class AndThenArgsParser[R1,R2,+R3](lhs:ArgsParser[R1], rhs:ArgsParser[R2], f:(R1,R2)⇒R3) extends ArgsParser[R3] {
    def apply(args:List[String]):ArgsParserResult[R3] = lhs(args) match {
      case failure:ArgsParserFailure ⇒ failure
      case ArgsParserSuccess(r1, remaining1) ⇒ rhs(remaining1) match {
        case failure:ArgsParserFailure ⇒ failure
        case ArgsParserSuccess(r2, remaining2) ⇒ ArgsParserSuccess(f(r1, r2), remaining2)
      }
    }
  }
  private[cli] final case class MapArgsParser[R1,+R2](inner:ArgsParser[R1], f:R1⇒R2) extends ArgsParser[R2] {
    def apply(args:List[String]):ArgsParserResult[R2] = inner(args) match {
      case failure:ArgsParserFailure ⇒ failure
      case ArgsParserSuccess(r1, remainingArgs) ⇒ ArgsParserSuccess(f(r1), remainingArgs)
    }
  }
  private[cli] final case class OptArgsParser[+R](inner:ArgsParser[R]) extends ArgsParser[Option[R]] {
    def apply(args:List[String]):ArgsParserResult[Option[R]] = inner(args) match {
      case ArgsParserSuccess(result, remainingArgs) ⇒ ArgsParserSuccess(Some(result), remainingArgs)
      case failure:ArgsParserFailure if failure hasPriority ⇒ failure
      case _:ArgsParserFailure ⇒ ArgsParserSuccess(None, args)
    }
  }
  private[cli] final case class OrArgsParser[+R](lhs:ArgsParser[R], rhs:ArgsParser[R], combineFailures:Boolean=true) extends ArgsParser[R] {
    def apply(args:List[String]):ArgsParserResult[R] = (lhs(args), rhs(args)) match {
      case (lf:ArgsParserFailure, rf:ArgsParserFailure) ⇒ if(combineFailures) lf combineOr rf else rf
      case (r:ArgsParserSuccess[R], _:ArgsParserFailure) ⇒ r
      case (_:ArgsParserFailure, r:ArgsParserSuccess[R]) ⇒ r
      case (lr:ArgsParserSuccess[R], rr:ArgsParserSuccess[R]) ⇒
        if(rr.remainingArgs.length < lr.remainingArgs.length) rr else lr
    }
  }
  private[cli] final case class PriorityFailureParser[+R](inner:ArgsParser[R]) extends ArgsParser[R] {
    def apply(args:List[String]):ArgsParserResult[R] = inner(args) match {
      case failure:ArgsParserFailure ⇒ failure withPriority
      case x ⇒ x
    }
  }
}
