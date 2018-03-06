package net.jackadull.dirish.op.util

import net.jackadull.dirish.op.combinator.CombinatorStyle

import scala.language.higherKinds

trait UsingCombinator[V[+_,+_]] {
  protected def combinatorStyle:CombinatorStyle[V]

  protected def v[R](f: ⇒R):V[R,Nothing] = combinatorStyle resultIn f
  protected def vtry[R,E](f: ⇒R, err:PartialFunction[Throwable,E]):V[R,E] =
    try {v(f)} catch {case exc if err isDefinedAt exc ⇒ combinatorStyle failWith err(exc)}
}
