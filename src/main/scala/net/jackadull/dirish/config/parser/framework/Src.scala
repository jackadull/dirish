package net.jackadull.dirish.config.parser.framework

trait Src[S[+_]] {
  def apply(s:S[Any], char:Char):S[Unit]
  def apply(s:S[Any], string:String):S[Unit]
  def copy[A](from:S[A], to:S[Any]):S[A]
  def fail[A](s:S[A], message:String):S[A]
  def flatMap[A,A2](s:S[A])(f:A=>S[A2]):S[A2]
  def isSuccess(s:S[Any]):Boolean
  def map[A,A2](s:S[A])(f:A=>A2):S[A2]
  def set[A](s:S[Any], v:A):S[A]
}
