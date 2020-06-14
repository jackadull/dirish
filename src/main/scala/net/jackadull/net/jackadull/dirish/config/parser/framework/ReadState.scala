package net.jackadull.net.jackadull.dirish.config.parser.framework

import net.jackadull.net.jackadull.dirish.config.parser.framework.ParseResult.{ParseError, ParseFailure, ParseSuccess}

sealed trait ReadState {
  def expecting(char:Char):ParseResult[Unit]
  def expecting(string:String, index:Int=0):ParseResult[Unit]
  def expectingEOF:ParseResult[Unit]

  def failure():ParseFailure = ParseFailure()
  def success[A](result:A):ParseSuccess[A] = ParseSuccess(result, this)
}
object ReadState {
  trait BeforeChar extends ReadState {
    def char:Char
    override def expecting(char:Char):ParseResult[Unit] =
      if(char == this.char) ParseSuccess((), next) else ParseFailure()
    override def expecting(string:String, index:Int):ParseResult[Unit] =
      if(index >= string.length) ParseSuccess((), next)
      else if(char == this.char) next.expecting(string, index+1)
      else ParseFailure()
    override def expectingEOF:ParseResult[Unit] = ParseFailure()
    def next:ReadState
  }

  final case class EOF() extends ReadState {
    override def expecting(char:Char):ParseResult[Unit] = ParseFailure()
    override def expecting(string:String, index:Int):ParseResult[Unit] =
      if(index >= string.length) ParseSuccess((), this) else ParseFailure()
    override def expectingEOF:ParseResult[Unit] = ParseSuccess((), this)
  }

  trait ReadError extends ReadState {
    override def expecting(char:Char):ParseResult[Unit] = ParseError()
    override def expecting(string:String, index:Int):ParseResult[Unit] = ParseError()
    override def expectingEOF:ParseResult[Unit] = ParseError()
  }
}
