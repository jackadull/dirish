package net.jackadull.net.jackadull.dirish.config.parser.framework

trait <=>[A,B] {
  def to:A=>B
  def from:B=>A
}
object <=> {
  def apply[A,B](to:A=>B, from:B=>A):A<=>B = Impl(to, from)

  private final case class Impl[A,B](to:A=>B, from:B=>A) extends (A<=>B)
}
