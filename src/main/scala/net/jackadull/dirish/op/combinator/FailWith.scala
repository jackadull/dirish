package net.jackadull.dirish.op.combinator

import net.jackadull.dirish.op.Op

import scala.language.higherKinds

final case class FailWith[+E](error:E) extends Op[Nothing,E,CombinatorStyle] {
  def instantiateIn[V[+_,+_]](style:CombinatorStyle[V]):V[Nothing,E] = style failWith error
}
