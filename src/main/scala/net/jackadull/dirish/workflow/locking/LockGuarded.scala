package net.jackadull.dirish.workflow.locking

import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.combinator.FailWith
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.higherKinds

final case class LockGuarded[+R,-S[V[+_,+_]]<:LockingStyle[V]](guarded:Op[R,OpError,S]) extends ProxyOp[R,OpError,S] {
  protected def innerOp:Op[R,OpError,S] =
    ((ObtainLock ~> guarded) #>>[R,OpError,S] {error ⇒ FreeLock ~> FailWith(error)}) <~ FreeLock
}
