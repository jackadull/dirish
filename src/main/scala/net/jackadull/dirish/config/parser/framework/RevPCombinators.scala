package net.jackadull.dirish.config.parser.framework

import RevP.Matcher

trait RevPCombinators[A] {
  this:RevP[A]=>

  def ?< :RevP[Option[A]] = RevPCombinators.?<(this)
  def *< :RevP[Seq[A]] = RevPCombinators.*<(this)
  def +< :RevP[Seq[A]] = RevPCombinators.+<(this)

  def map[B](f:A<=>B):RevP[B] = RevPCombinators.map(this, f)
  def ~>[B](that:RevP[B]):RevP[B] = RevPCombinators.~>(this, that)
  def <~[B](that:RevP[B]):RevP[A] = RevPCombinators.<~(this, that)
  def <:>[B](that:RevP[B]):RevP[(A,B)] = RevPCombinators.<:>(this, that)

  def ? :Matcher = RevPCombinators.?(this)
  def * :Matcher = RevPCombinators.*(this)
  def + :Matcher = RevPCombinators.+(this)

  def ~(that:RevP[_]):Matcher = RevPCombinators.~(this, that)
  def |[B](that:RevP[B]):Matcher = RevPCombinators.|(this, that)
}
private object RevPCombinators {
  import RevP._
  import Matcher._

  def ?<[A](p:RevP[A]):RevP[Option[A]] = ParseOpt(p)
  def *<[A](p:RevP[A]):RevP[Seq[A]] = ParseRep(p)
  def +<[A](p:RevP[A]):RevP[Seq[A]] = ParseRep1(p)

  def map[A,B](p:RevP[A], f:A<=>B):RevP[B] = p match {
    case Mapped(p0, f0) => Mapped(p0, f0.andThen(f))
    case _ => Mapped(p, f)
  }

  def ~>[A,B](p1:RevP[A], p2:RevP[B]):RevP[B] = p1.matcher <:> p2 map <=>.tuple2_2(())
  def <~[A,B](p1:RevP[A], p2:RevP[B]):RevP[A] = p1 <:> p2.matcher map <=>.tuple2_1(())
  def <:>[A,B](p1:RevP[A], p2:RevP[B]):RevP[(A,B)] = ParseTuple2(p1, p2)

  def ?[A](p:RevP[A]):Matcher = p.matcher match {
    case m@MatchOpt(_) => m
    case m@MatchRep(_) => m
    case m => MatchOpt(m)
  }

  def *[A](p:RevP[A]):Matcher = p.matcher match {
    case MatchOpt(optional) => optional.*
    case m@MatchRep(_) => m
    case m => MatchRep(m)
  }

  def +[A](p:RevP[A]):Matcher = p.matcher match {
    case MatchOpt(optional) => optional.*
    case m@MatchRep(_) => m
    case m => m ~ m.*
  }

  def ~[A,B](p1:RevP[A], p2:RevP[B]):Matcher = (p1.matcher, p2.matcher) match {
    case (MatchSeq(s1), MatchSeq(s2)) => MatchSeq(s1 ++ s2)
    case (MatchSeq(s1), m2) => MatchSeq(s1 :+ m2)
    case (m1, MatchSeq(s2)) => MatchSeq(m1 +: s2)
    case (m1, m2) => MatchSeq(List(m1, m2))
  }

  def |[A,B](p1:RevP[A], p2:RevP[B]):Matcher = (p1.matcher, p2.matcher) match {
    case (MatchAlt(a1), MatchAlt(a2)) => MatchAlt(a1 ++ a2)
    case (MatchAlt(a1), m2) => MatchAlt(a1 :+ m2)
    case (m1, MatchAlt(a2)) => MatchAlt(m1 +: a2)
    case (m1, m2) => MatchAlt(List(m1, m2))
  }
}
