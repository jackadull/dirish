package net.jackadull.net.jackadull.dirish.config.parser.framework

import scala.language.implicitConversions

trait <=>[A,B] {
  def to:A=>B
  def from:B=>A

  def andThen[B2](that:B<=>B2):A<=>B2
}
object <=> {
  def apply[A,B](to:A=>B, from:B=>A):A<=>B = Impl(to, from)
  def symmetric[A](f:A=>A):A<=>A = Impl(f, f)

  private final case class Impl[A,B](to:A=>B, from:B=>A) extends (A<=>B) {
    override def andThen[B2](that:B<=>B2):A<=>B2 = Impl(to.andThen(that.to), that.from.andThen(from))
  }
}
