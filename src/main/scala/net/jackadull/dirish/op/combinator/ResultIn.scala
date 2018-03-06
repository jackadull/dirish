package net.jackadull.dirish.op.combinator

import net.jackadull.dirish.op.Op

import scala.language.higherKinds

final case class ResultIn[+R](v:R) extends Op[R,Nothing,CombinatorStyle] {
  def instantiateIn[V[+_,+_]](style:CombinatorStyle[V]):V[R,Nothing] = style resultIn v
}
object ResultIn {
  val success:ResultIn[Unit] = ResultIn(())
}
