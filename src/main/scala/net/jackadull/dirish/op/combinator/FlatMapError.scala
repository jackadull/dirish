package net.jackadull.dirish.op.combinator

import net.jackadull.dirish.op.Op

import scala.language.higherKinds

final case class FlatMapError[+R,E1,+E2,-S[V[+_,+_]]<:CombinatorStyle[V]](v:Op[R,E1,S], f:E1⇒Op[R,E2,S]) extends Op[R,E2,S] {
  def instantiateIn[V[+_,+_]](style:S[V]):V[R,E2] =
    style flatMapError (v instantiateIn style, f andThen {_ instantiateIn style})
}
