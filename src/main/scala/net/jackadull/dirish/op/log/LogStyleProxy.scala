package net.jackadull.dirish.op.log

import scala.language.higherKinds

trait LogStyleProxy[V[+_,+_]] extends LogStyle[V] {
  protected def logStyle:LogStyle[V]

  def log(logger:String, level:Log.OpLogLevel, message:String, throwableOpt:Option[Throwable]):V[Unit,Nothing] = logStyle log (logger, level, message, throwableOpt)
}
