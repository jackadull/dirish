package net.jackadull.dirish.op.network

import net.jackadull.dirish.op.Op

import scala.language.higherKinds

final case class CanConnectToHost(host:String, port:Int, timeoutMillis:Int) extends Op[Boolean,CanConnectToHostError,NetworkStyle] {
  def instantiateIn[V[+_,+_]](style:NetworkStyle[V]):V[Boolean,CanConnectToHostError] = style canConnectToHost (host, port, timeoutMillis)
}
