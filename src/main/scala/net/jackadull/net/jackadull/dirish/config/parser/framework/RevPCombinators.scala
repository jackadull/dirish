package net.jackadull.net.jackadull.dirish.config.parser.framework

import net.jackadull.net.jackadull.dirish.config.parser.framework.RevP3.Matcher

trait RevPCombinators[A] {
  this:RevP3[A]=>

  def map[B](f:A<=>B):RevP3[B] = RevPCombinators.map(this, f)
  def ~>[B](that:RevP3[B]):RevP3[B] = RevPCombinators.~>(this, that)
  def <~[B](that:RevP3[B]):RevP3[A] = RevPCombinators.<~(this, that)
  def <:>[B](that:RevP3[B]):RevP3[(A,B)] = RevPCombinators.<:>(this, that)

  def ? :Matcher = RevPCombinators.?(this)

  def ~(that:RevP3[_]):Matcher = RevPCombinators.~(this, that)
  def |[B](that:RevP3[B]):Matcher = RevPCombinators.|(this, that)
}
private object RevPCombinators {
  import RevP3._
  import Matcher._

  def map[A,B](p:RevP3[A], f:A<=>B):RevP3[B] = p match {
    case Mapped(p0, f0) => Mapped(p0, f0.andThen(f))
    case _ => Mapped(p, f)
  }

  def ~>[A,B](p1:RevP3[A], p2:RevP3[B]):RevP3[B] = p1.matcher <:> p2 map <=>.tuple2_2(())

  def <~[A,B](p1:RevP3[A], p2:RevP3[B]):RevP3[A] = p1 <:> p2.matcher map <=>.tuple2_1(())

  def <:>[A,B](p1:RevP3[A], p2:RevP3[B]):RevP3[(A,B)] = ParseTuple2(p1, p2)

  def ?[A](p:RevP3[A]):Matcher = p.matcher match {
    case m@MatchOpt(_) => m
    case m => MatchOpt(m)
  }

  def ~[A,B](p1:RevP3[A], p2:RevP3[B]):Matcher = (p1.matcher, p2.matcher) match {
    case (MatchSeq(s1), MatchSeq(s2)) => MatchSeq(s1 ++ s2)
    case (MatchSeq(s1), m2) => MatchSeq(s1 :+ m2)
    case (m1, MatchSeq(s2)) => MatchSeq(m1 +: s2)
    case (m1, m2) => MatchSeq(List(m1, m2))
  }

  def |[A,B](p1:RevP3[A], p2:RevP3[B]):Matcher = (p1.matcher, p2.matcher) match {
    case (MatchAlt(a1), MatchAlt(a2)) => MatchAlt(a1 ++ a2)
    case (MatchAlt(a1), m2) => MatchAlt(a1 :+ m2)
    case (m1, MatchAlt(a2)) => MatchAlt(m1 +: a2)
    case (m1, m2) => MatchAlt(List(m1, m2))
  }
}
