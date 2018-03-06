package net.jackadull.dirish.op.network

import scala.language.higherKinds

trait NetworkStyle[V[+_,+_]] {
  def isHostReachable(host:String, timeoutMillis:Int):V[Boolean,IsHostReachableError]
}
