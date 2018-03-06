package net.jackadull.dirish.op

import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, CombinatorStyle, EitherV}
import net.jackadull.dirish.op.git.{BlockingGitStyle, GitStyle}
import net.jackadull.dirish.op.io.{BlockingIOStyle, IOStyle}
import net.jackadull.dirish.op.log.{BlockingSLF4JLogStyle, LogStyle}
import net.jackadull.dirish.op.network.{BlockingNetworkStyle, NetworkStyle}
import net.jackadull.dirish.op.settings.{DefaultDirishSettingStyle, DirishSettingStyle}
import net.jackadull.dirish.op.signals.{BlockingSignalStyle, SignalStyle}

trait BlockingEitherStyles extends StyleProxies[EitherV]
object BlockingEitherStyles extends StyleProxies[EitherV] {
  protected def combinatorStyle:CombinatorStyle[EitherV] = BlockingEitherCombinatorStyle
  protected def gitStyle:GitStyle[EitherV] = BlockingGitStyle
  protected def ioStyle:IOStyle[EitherV] = BlockingIOStyle
  protected def logStyle:LogStyle[EitherV] = BlockingSLF4JLogStyle
  protected def networkStyle:NetworkStyle[EitherV] = BlockingNetworkStyle
  protected def settingsStyle:DirishSettingStyle[EitherV] = DefaultDirishSettingStyle
  protected def signalStyle:SignalStyle[EitherV] = BlockingSignalStyle
}
