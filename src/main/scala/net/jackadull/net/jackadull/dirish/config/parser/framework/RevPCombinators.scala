package net.jackadull.net.jackadull.dirish.config.parser.framework

import net.jackadull.net.jackadull.dirish.config.parser.framework.RevP3.Matcher

trait RevPCombinators[A] {
  this:RevP3[A]=>

  def map[B](f:A<=>B):RevP3[B] = RevPCombinators.map(this, f)
  def ~(that:RevP3[_]):Matcher = RevPCombinators.~(this, that)
  def ~>[B](that:RevP3[B]):RevP3[B] = RevPCombinators.~>(this, that)
  def <~[B](that:RevP3[B]):RevP3[A] = RevPCombinators.<~(this, that)
  def <:>[B](that:RevP3[B]):RevP3[(A,B)] = RevPCombinators.<:>(this, that)
}
private object RevPCombinators {
  import RevP3._
  import Matcher._

  def map[A,B](p:RevP3[A], f:A<=>B):RevP3[B] = p match {
    case Mapped(p0, f0) => Mapped(p0, f0.andThen(f))
    case _ => Mapped(p, f)
  }

  def ~[A,B](p1:RevP3[A], p2:RevP3[B]):Matcher = (p1.matcher, p2.matcher) match {
    case (Sequence(s1), Sequence(s2)) => Sequence(s1 ++ s2)
    case (Sequence(s1), m2) => Sequence(s1 :+ m2)
    case (m1, Sequence(s2)) => Sequence(m1 +: s2)
    case (m1, m2) => Sequence(List(m1, m2))
  }

  def ~>[A,B](p1:RevP3[A], p2:RevP3[B]):RevP3[B] = p1.matcher <:> p2 map <=>.tuple2_2(())

  def <~[A,B](p1:RevP3[A], p2:RevP3[B]):RevP3[A] = p1 <:> p2.matcher map <=>.tuple2_1(())

  def <:>[A,B](p1:RevP3[A], p2:RevP3[B]):RevP3[(A,B)] = ResolveTuple2(p1, p2)
}
