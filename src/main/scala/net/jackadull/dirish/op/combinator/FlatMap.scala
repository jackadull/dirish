package net.jackadull.dirish.op.combinator

import net.jackadull.dirish.op.Op

import scala.annotation.tailrec
import scala.language.{higherKinds, postfixOps}

final case class FlatMap[R1,+R2,+E,-S[V[+_,+_]]<:CombinatorStyle[V]](v:Op[R1,E,S], f:R1⇒Op[R2,E,S]) extends Op[R2,E,S] {
  def instantiateIn[V[+_,+_]](style:S[V]):V[R2,E] = style flatMap (v instantiateIn style, f andThen (_ instantiateIn style))

  /* TODO debug and fix:
  def instantiateIn[V[+_,+_]](style:S[V]):V[R2,E] =
    if(v.isInstanceOf[FlatMap[_,_,_,S]]) FlatMap.nestedInstantiate(this, style)
    else style.flatMap(v instantiateIn style, {v2:R1 ⇒ f(v2) instantiateIn style})
    */
}
object FlatMap {
  private def nestedInstantiate[R1,R2,E,V[+_,+_],S[V2[+_,+_]]<:CombinatorStyle[V2]](fm:FlatMap[R1,R2,E,S], style:S[V]):V[R2,E] = {
    val (init, fs) = fSequence(fm, style)
    val result = applyFSequence(init, fs, style)
    result.asInstanceOf[V[R2,E]]
  }

  @tailrec private def fSequence[R1,R2,E,V[+_,+_],S[V2[+_,+_]]<:CombinatorStyle[V2]](fm:FlatMap[R1,R2,E,S], style:S[V], soFar:Vector[Any⇒V[Any,Any]]=Vector()):(()⇒V[Any,Any],Vector[Any⇒V[Any,Any]]) =
    fm.v match {
      case fm2:FlatMap[_,R1,_,S] ⇒ fSequence(fm2, style, soFar :+ (fm.f andThen {_ instantiateIn style}).asInstanceOf[Any⇒V[Any,Any]])
      case bottom:Op[_,_,S] ⇒ ({() ⇒ bottom instantiateIn style}, soFar)
    }

  private def applyFSequence[V[+_,+_],S[V2[+_,+_]]<:CombinatorStyle[V2]](init:()⇒V[Any,Any], fs:Iterable[Any⇒V[Any,Any]], style:S[V]):V[Any,Any] = {
    var x:V[Any,Any] = init()
    val i = fs.iterator
    while((i hasNext) && !(style isDeterminedAsFailed x)) x = style.flatMap(x, i.next())
    x
  }
}
