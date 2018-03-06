package net.jackadull.dirish.op

package object combinator {
  type EitherV[+R,+E] = Either[E,R]
  type EitherCombinatorStyle = CombinatorStyle[EitherV]
}
