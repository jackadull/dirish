package net.jackadull.dirish.op.network

import net.jackadull.dirish.op.Op

import scala.language.higherKinds

final case class IsHostReachable(host:String, timeoutMillis:Int) extends Op[Boolean,IsHostReachableError,NetworkStyle] {
  def instantiateIn[V[+_,+_]](style:NetworkStyle[V]):V[Boolean,IsHostReachableError] =
    style isHostReachable (host, timeoutMillis)
}
