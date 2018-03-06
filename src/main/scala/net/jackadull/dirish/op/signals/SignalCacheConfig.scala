package net.jackadull.dirish.op.signals

import scala.concurrent.duration.FiniteDuration

final case class SignalCacheConfig(cacheDuration:FiniteDuration, errorCacheDuration:FiniteDuration)
