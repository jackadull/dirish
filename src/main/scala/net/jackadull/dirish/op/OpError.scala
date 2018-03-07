package net.jackadull.dirish.op

import net.jackadull.dirish.op.git.GenericGitError
import net.jackadull.dirish.op.io.GenericIOError
import net.jackadull.dirish.op.network.GenericNetworkError

trait OpError

trait GenericOpError extends GenericGitError with GenericIOError with GenericNetworkError

final case class GenericMessageError(msg:String) extends GenericOpError {override def toString = msg}
final case class GenericThrowableError(msg:String, throwable:Throwable) extends GenericOpError
