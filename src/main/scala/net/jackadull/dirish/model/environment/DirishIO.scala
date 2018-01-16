package net.jackadull.dirish.model.environment

import scala.language.higherKinds

trait DirishIO[A[_]] {
  def flatMap[B,C](io:A[B])(f:B⇒A[C]):A[C]
  def map[B,C](io:A[B])(f:B⇒C):A[C]
  def unit[B](v: ⇒B):A[B]
}
