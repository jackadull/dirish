package net.jackadull.net.jackadull.dirish.config.parser.framework

trait Src[S[+_]] {
  def apply[A](s:S[A], char:Char):S[A]
  def carry[A](empty:S[Any], payload:S[A]):S[A]
  def isError(s:S[Any]):Boolean // TODO is this needed?
  def isFailure(s:S[Any]):Boolean // TODO is this needed?
  def isSuccess(s:S[Any]):Boolean
  def orElse[A](s:S[A], onFailure: =>S[A]):S[A]
}
