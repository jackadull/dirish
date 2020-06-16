package net.jackadull.net.jackadull.dirish.config.parser.framework

trait Src[S[+_]] {
  def apply(s:S[_], char:Char):S[Nothing]
  def isError(s:S[_]):Boolean // TODO is this needed?
  def isFailure(s:S[_]):Boolean // TODO is this needed?
  def isSuccess(s:S[_]):Boolean
  def orElse[A](s:S[A], onFailure: =>S[A]):S[A]
}
