package net.jackadull.dirish.op.combinator

import scala.language.higherKinds

trait CombinatorStyleProxy[V[+_,+_]] extends CombinatorStyle[V] {
  protected def combinatorStyle:CombinatorStyle[V]

  def failWith[E](e: ⇒E):V[Nothing,E] = combinatorStyle failWith e
  def flatMap[R1,R2,E](v:V[R1,E], f:R1⇒V[R2,E]):V[R2,E] = combinatorStyle flatMap (v, f)
  def flatMapError[R,E1,E2](v:V[R,E1], f:E1⇒V[R,E2]):V[R,E2] = combinatorStyle flatMapError (v, f)
  def isDeterminedAsFailed[R,E](v:V[R,E]):Boolean = combinatorStyle isDeterminedAsFailed v
  def resultIn[R](v: ⇒R):V[R,Nothing] = combinatorStyle resultIn v
}
