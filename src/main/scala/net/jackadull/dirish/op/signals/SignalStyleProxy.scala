package net.jackadull.dirish.op.signals

import net.jackadull.dirish.op.signals.SignalStyle.SignalGet

import scala.language.higherKinds

trait SignalStyleProxy[V[+_,+_]] extends SignalStyle[V] {
  protected def signalStyle:SignalStyle[V]

  def signal[R,E](get:SignalGet[R,E,V], caching:SignalCacheConfig):V[R,E] = signalStyle signal (get, caching)
}
