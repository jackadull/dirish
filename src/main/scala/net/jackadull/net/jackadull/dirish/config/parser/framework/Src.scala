package net.jackadull.net.jackadull.dirish.config.parser.framework

trait Src[S[+_]] {
  def apply(s:S[Nothing], char:Char):S[Nothing]
  def carry[A](empty:S[Nothing], payload:S[A]):S[A]
  def empty(s:S[_]):S[Nothing]
  def isError(s:S[Nothing]):Boolean // TODO is this needed?
  def isFailure(s:S[Nothing]):Boolean // TODO is this needed?
  def isSuccess(s:S[Nothing]):Boolean
  def orElse[A](s:S[A], onFailure: =>S[A]):S[A]
}
