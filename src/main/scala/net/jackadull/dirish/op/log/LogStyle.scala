package net.jackadull.dirish.op.log

import net.jackadull.dirish.op.log.Log.OpLogLevel

import scala.language.higherKinds

trait LogStyle[V[+_,+_]] {
  def log(logger:String, level:OpLogLevel, message:String, throwableOpt:Option[Throwable]):V[Unit,Nothing]
}
