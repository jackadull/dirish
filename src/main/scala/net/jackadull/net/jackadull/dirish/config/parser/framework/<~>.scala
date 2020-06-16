package net.jackadull.net.jackadull.dirish.config.parser.framework

// TODO remove if unused
trait <~>[A,B] {
  def to:PartialFunction[A,B]
  def from:PartialFunction[B,A]
}
object <~> {
  def apply[A,B](to:PartialFunction[A,B], from:PartialFunction[B,A]):A<~>B = Impl(to, from)

  private final case class Impl[A,B](to:PartialFunction[A,B], from:PartialFunction[B,A]) extends (A<~>B)
}
