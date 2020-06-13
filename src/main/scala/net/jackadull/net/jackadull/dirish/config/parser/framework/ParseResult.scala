package net.jackadull.net.jackadull.dirish.config.parser.framework

sealed trait ParseResult[+A] {
  def flatMap[A2](f:(A,ReadState)=>ParseResult[A2]):ParseResult[A2]
  def mapResult[A2](f:A=>A2):ParseResult[A2]
  def orElse[A2>:A](otherwise: =>ParseResult[A2]):ParseResult[A2]
}
object ParseResult {
  sealed trait NoSuccess extends ParseResult[Nothing] {
    override def flatMap[A2](f:(Nothing,ReadState)=>ParseResult[A2]):ParseResult[A2] = this
    override def mapResult[A2](f:Nothing=>A2):ParseResult[A2] = this
  }

  final case class ParseError() extends NoSuccess {
    override def orElse[A2>:Nothing](otherwise: =>ParseResult[A2]):ParseResult[A2] = this
  }
  final case class ParseFailure() extends NoSuccess {
    override def orElse[A2>:Nothing](otherwise: =>ParseResult[A2]):ParseResult[A2] = otherwise
  }
  final case class ParseSuccess[A](result:A, next:ReadState) extends ParseResult[A] {
    override def flatMap[A2](f:(A,ReadState)=>ParseResult[A2]):ParseResult[A2] = f(result, next)
    override def mapResult[A2](f:A=>A2):ParseResult[A2] = copy(result = f(result))
    override def orElse[A2>:A](otherwise: =>ParseResult[A2]):ParseResult[A2] = this
  }
}
