package net.jackadull.dirish.op.log

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.op.log.Log.OpLogLevel

import scala.language.higherKinds

final case class Log(logger:String, level:OpLogLevel, message:String, throwableOpt:Option[Throwable]=None) extends Op[Unit,Nothing,LogStyle] {
  def instantiateIn[V[+_,+_]](style:LogStyle[V]):V[Unit,Nothing] = style log (logger, level, message, throwableOpt)
}
object Log {
  final case class about(logger:String) {
    def debug(message:String):Log = Log.debug(logger, message)
    def debug(message:String, throwable:Throwable):Log = Log.debug(logger, message, throwable)
    def error(message:String):Log = Log.error(logger, message)
    def error(message:String, throwable:Throwable):Log = Log.error(logger, message, throwable)
    def info(message:String):Log = Log.info(logger, message)
    def info(message:String, throwable:Throwable):Log = Log.info(logger, message, throwable)
    def trace(message:String):Log = Log.trace(logger, message)
    def trace(message:String, throwable:Throwable):Log = Log.trace(logger, message, throwable)
    def warn(message:String):Log = Log.warn(logger, message)
    def warn(message:String, throwable:Throwable):Log = Log.warn(logger, message, throwable)
  }
  object about {
    def apply(obj:AnyRef):about = about(if(obj==null) "ROOT" else obj.getClass.getName)
  }

  sealed trait OpLogLevel {
    def apply(logger:String, message:String):Log = Log(logger, this, message)
    def apply(logger:String, message:String, throwable:Throwable):Log = Log(logger, this, message, Option(throwable))
  }

  object debug extends OpLogLevel
  object error extends OpLogLevel
  object info extends OpLogLevel
  object trace extends OpLogLevel
  object warn extends OpLogLevel
}
