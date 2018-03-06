package net.jackadull.dirish.op.network

import scala.language.higherKinds

trait NetworkStyleProxy[V[+_,+_]] extends NetworkStyle[V] {
  protected def networkStyle:NetworkStyle[V]

  def isHostReachable(host:String, timeoutMillis:Int):V[Boolean,IsHostReachableError] = networkStyle isHostReachable (host, timeoutMillis)
}
