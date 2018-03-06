package net.jackadull.dirish.op.signals

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.op.signals.SignalStyle.SignalGet.SignalGetImpl

import scala.language.higherKinds

final case class Signal[+R,+E,-S[V[+_,+_]]<:SignalStyle[V]](get:Op[R,E,S], caching:SignalCacheConfig) extends Op[R,E,S] {
  def instantiateIn[V[+_,+_]](style:S[V]):V[R,E] = style signal (SignalGetImpl(get, style), caching)
}
