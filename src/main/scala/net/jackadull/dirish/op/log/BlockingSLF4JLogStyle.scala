package net.jackadull.dirish.op.log

import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, CombinatorStyle, EitherV}
import net.jackadull.dirish.op.log.Log.OpLogLevel
import net.jackadull.dirish.op.util.UsingCombinator
import org.slf4j.LoggerFactory

import scala.language.{higherKinds, reflectiveCalls}

class BlockingSLF4JLogStyle[V[+_,+_]](protected val combinatorStyle:CombinatorStyle[V])
extends LogStyle[V] with UsingCombinator[V] {
  def log(logger:String, level:OpLogLevel, message:String, throwableOpt:Option[Throwable]):V[Unit,Nothing] = v {
    val l = LoggerFactory getLogger logger
    (level, throwableOpt) match {
      case (Log.debug, None) ⇒ l debug message
      case (Log.debug, Some(t)) ⇒ l debug (message, t)
      case (Log.error, None) ⇒ l error message
      case (Log.error, Some(t)) ⇒ l error (message, t)
      case (Log.info, None) ⇒ l info message
      case (Log.info, Some(t)) ⇒ l info (message, t)
      case (Log.trace, None) ⇒ l trace message
      case (Log.trace, Some(t)) ⇒ l trace (message, t)
      case (Log.warn, None) ⇒ l warn message
      case (Log.warn, Some(t)) ⇒ l warn (message, t)
    }
  }
}
object BlockingSLF4JLogStyle extends BlockingSLF4JLogStyle[EitherV](BlockingEitherCombinatorStyle)
