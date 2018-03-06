package net.jackadull.dirish.op.combinator

import scala.language.higherKinds

trait CombinatorStyle[V[+_,+_]] {
  def failWith[E](e: ⇒E):V[Nothing,E]
  def flatMap[R1,R2,E](v:V[R1,E], f:R1⇒V[R2,E]):V[R2,E]
  def flatMapError[R,E1,E2](v:V[R,E1], f:E1⇒V[R,E2]):V[R,E2]
  /** Return `true` when the given value can be determined to represent a failure (in a non-blocking way), or
   * `false` when the failure state can either not be determined in a non-blocking way, or the given value represents a
   * success.
   *
   * This method is intended to help optimization, by enabling callers to finish failed chains of operations early. */
  def isDeterminedAsFailed[R,E](v:V[R,E]):Boolean
  def resultIn[R](v: ⇒R):V[R,Nothing]
}
