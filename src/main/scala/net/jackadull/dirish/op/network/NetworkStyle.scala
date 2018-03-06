package net.jackadull.dirish.op.network

import scala.language.higherKinds

trait NetworkStyle[V[+_,+_]] {
  def canConnectToHost(host:String, port:Int, timeoutMillis:Int):V[Boolean,CanConnectToHostError]
  def isHostReachable(host:String, timeoutMillis:Int):V[Boolean,IsHostReachableError]
}
