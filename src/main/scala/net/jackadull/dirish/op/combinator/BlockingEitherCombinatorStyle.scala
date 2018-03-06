package net.jackadull.dirish.op.combinator

import scala.language.{postfixOps, reflectiveCalls}

trait BlockingEitherCombinatorStyle extends EitherCombinatorStyle {
  def failWith[E](e: ⇒E):Either[E,Nothing] = Left(e)
  def flatMap[R1,R2,E](v:Either[E,R1], f:R1⇒Either[E,R2]):Either[E,R2] = v.right flatMap f
  def flatMapError[R,E1,E2](v:Either[E1,R], f:E1⇒Either[E2,R]):Either[E2,R] = v.left flatMap f
  def isDeterminedAsFailed[R,E](v:Either[E,R]):Boolean = v isLeft
  def resultIn[R](v: ⇒R):Either[Nothing,R] = Right(v)
}
object BlockingEitherCombinatorStyle extends BlockingEitherCombinatorStyle
