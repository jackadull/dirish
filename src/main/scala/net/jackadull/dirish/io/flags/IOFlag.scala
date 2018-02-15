package net.jackadull.dirish.io.flags

import net.jackadull.dirish.io.{BooleanIOResult, GenericIOError, GetFlagStatusResult, IO}

import scala.concurrent.duration.FiniteDuration
import scala.language.higherKinds

sealed trait IOFlag {
  private[io] def check[I[+_]](io:IO[I]):I[GetFlagStatusResult]
}
final case class CachedIOFlag(uncached:IOFlag, timeToLive:FiniteDuration) extends IOFlag {
  private[io] def check[I[+_]](io:IO[I]) = uncached.check(io)
}

final case class IsHostReachableFlag(host:String, timeoutMillis:Int) extends IOFlag {
  private[io] def check[I[+_]](io:IO[I]) = io.map(io isHostReachable (host, timeoutMillis)) {
    case b:BooleanIOResult ⇒ b
    case err:GenericIOError ⇒ err
  }
}
