package net.jackadull.net.jackadull.dirish.config.parser.framework

trait <=>[A,B] {
  def to:A=>B
  def from:B=>A

  def andThen[B2](that:B<=>B2):A<=>B2
}
object <=> {
  def apply[A,B](to:A=>B, from:B=>A):A<=>B = Impl(to, from)

  private final case class Impl[A,B](to:A=>B, from:B=>A) extends (A<=>B) {
    override def andThen[B2](that:B<=>B2):A<=>B2 = Impl(to.andThen(that.to), that.from.andThen(from))
  }
}
