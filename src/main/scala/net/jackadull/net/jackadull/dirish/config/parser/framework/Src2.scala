package net.jackadull.net.jackadull.dirish.config.parser.framework

trait Src2[S[+_]] {
  def apply(s:S[Any], char:Char):S[Unit]
  def copy[A](from:S[A], to:S[Any]):S[A]
  def flatMap[A,A2](s:S[A])(f:A=>S[A2]):S[A2]
  def isSuccess(s:S[Any]):Boolean
  def map[A,A2](s:S[A])(f:A=>A2):S[A2]
  def unit(s:S[Any]):S[Unit]
}
