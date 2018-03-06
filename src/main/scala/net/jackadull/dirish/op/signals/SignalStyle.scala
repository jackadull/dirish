package net.jackadull.dirish.op.signals

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.op.signals.SignalStyle.SignalGet

import scala.language.higherKinds

trait SignalStyle[V[+_,+_]] {
  def signal[R,E](get:SignalGet[R,E,V], caching:SignalCacheConfig):V[R,E]
}
object SignalStyle {
  trait SignalGet[+R,+E,V[+_,+_]] {def apply():V[R,E]}
  object SignalGet {
    private[signals] final case class SignalGetImpl[+R,+E,V[+_,+_],S[V2[+_,+_]]<:SignalStyle[V2]](op:Op[R,E,S], style:S[V])
    extends SignalGet[R,E,V] {def apply():V[R,E] = op instantiateIn style}
  }
}
