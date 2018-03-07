package net.jackadull.dirish.op

import net.jackadull.dirish.op.combinator.{CombinatorStyle, CombinatorStyleProxy}
import net.jackadull.dirish.op.git.{GitStyle, GitStyleProxy}
import net.jackadull.dirish.op.io.{IOStyle, IOStyleProxy}
import net.jackadull.dirish.op.log.{LogStyle, LogStyleProxy}
import net.jackadull.dirish.op.network.{NetworkStyle, NetworkStyleProxy}
import net.jackadull.dirish.op.settings.{DirishSettingStyle, DirishSettingStyleProxy}
import net.jackadull.dirish.op.signals.{SignalStyle, SignalStyleProxy}

import scala.language.higherKinds

trait StyleProxies[V[+_,+_]] extends CombinatorStyleProxy[V] with DirishSettingStyleProxy[V] with GitStyleProxy[V]
with IOStyleProxy[V] with LogStyleProxy[V] with NetworkStyleProxy[V] with SignalStyleProxy[V]
object StyleProxies {
  def apply[V[+_,+_]](combinatorStyle:CombinatorStyle[V], gitStyle:GitStyle[V], ioStyle:IOStyle[V],
    logStyle:LogStyle[V], networkStyle:NetworkStyle[V], settingStyle:DirishSettingStyle[V],
    signalStyle:SignalStyle[V]):StyleProxies[V] = StyleProxiesImpl(combinatorStyle, gitStyle, ioStyle, logStyle,
      networkStyle, settingStyle, signalStyle)

  private case class StyleProxiesImpl[V[+_,+_]](combinatorStyle:CombinatorStyle[V], gitStyle:GitStyle[V],
    ioStyle:IOStyle[V], logStyle:LogStyle[V], networkStyle:NetworkStyle[V], settingsStyle:DirishSettingStyle[V],
    signalStyle:SignalStyle[V]) extends StyleProxies[V]
}
