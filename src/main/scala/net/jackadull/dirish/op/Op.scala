package net.jackadull.dirish.op

import net.jackadull.dirish.op.combinator._

import scala.language.{higherKinds, postfixOps}

/** Generic model for a potentially blocking operation. This is a general case of I/O, also applicable to other
 * areas. Type `R` represents the result of the operation, `E` is the type of a potential error (if an error happens),
 * and `S` is the type required for instantiating the operation to an implementation-specific executable term, called
 * "style" here.
 *
 * Before instantiating the operation, this is just an abstract model of what might be done, without any potentially
 * blocking execution logic. After instantiation, the result type is wrapped inside an implementation-specific type,
 * and how exactly to trigger execution and obtain the result is also implementation specific. */
trait Op[+R,+E,-S[_[+_,+_]]] {
  def instantiateIn[V[+_,+_]](style:S[V]):V[R,E]

  // Combinators

  def flatMap[R2,E2>:E,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](f:R⇒Op[R2,E2,S2]):Op[R2,E2,S2] =
    FlatMap[R,R2,E2,S2](this, f)
  def flatMapError[R2>:R,E2,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](f:E⇒Op[R2,E2,S2]):Op[R2,E2,S2] =
    FlatMapError[R2,E,E2,S2](this, f)
  def flatMapErrorPF[R2>:R,E2>:E,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](pf:PartialFunction[E,Op[R2,E2,S2]]):Op[R2,E2,S2] =
    flatMapError {
      case defined if pf isDefinedAt defined ⇒ pf(defined)
      case notDefined ⇒ FailWith(notDefined)
    }
  def foldLeft[R1,R2,E2>:E,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](z:R2)(op:(R2,R1)⇒R2)(implicit ev:R⇒Traversable[Op[R1,E2,S2]]):Op[R2,E2,S2] = {
    def innerOp1(r2op:Op[R2,E2,S2], r1op:Op[R1,E2,S2]):Op[R2,E2,S2] = r2op.flatMap[R2,E2,S2](r2 ⇒ r1op.flatMap (r1 ⇒ innerOp2(r2, r1op)))
    def innerOp2(r2:R2, r1op:Op[R1,E2,S2]):Op[R2,E2,S2] = r1op.flatMap(r1 ⇒ ResultIn(op(r2, r1)))
    flatMap {r ⇒ ev(r).foldLeft[Op[R2,E2,S2]](ResultIn(z))(innerOp1)}
  }
  def foreach[R1,E2>:E,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](f:R1⇒Op[Unit,E2,S2])(implicit ev:R⇒Traversable[R1]):Op[Unit,E2,S2] =
    flatMap({v ⇒ ev(v).foldLeft[Op[Unit,E2,S2]](ResultIn success) {(soFar, el) ⇒ soFar ~> f(el)}})

  // Syntactic sugar

  final def >>[R2,E2>:E,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](f:R⇒Op[R2,E2,S2]):Op[R2,E2,S2] = flatMap(f)
  final def ~>[R2,E2>:E,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](f: ⇒Op[R2,E2,S2]):Op[R2,E2,S2] = flatMap(_ ⇒ f)
  final def *>~[R1,E2>:E,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](f:R1⇒Op[Unit,E2,S2])(implicit ev:R⇒Traversable[R1]):Op[Unit,E2,S2] = foreach(f)

  final def <~[R2,E2>:E,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](f: ⇒Op[R2,E2,S2]):Op[R,E2,S2] = flatMap(r ⇒ f.flatMap[R,E2,S2](_ ⇒ ResultIn[R](r)))

  final def #>>[R2>:R,E2,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](f:E⇒Op[R2,E2,S2]):Op[R2,E2,S2] = flatMapError(f)
  final def #>>?[R2>:R,E2>:E,S2[V[+_,+_]]<:(S[V] with CombinatorStyle[V])](pf:PartialFunction[E,Op[R2,E2,S2]]):Op[R2,E2,S2] = flatMapErrorPF(pf)
}
object Op {
  trait ProxyOp[+R,+E,-S[_[+_,+_]]] extends Op[R,E,S] {
    def instantiateIn[V[+_,+_]](style:S[V]):V[R, E] = innerOp instantiateIn style
    protected def innerOp:Op[R,E,S]
  }
}
