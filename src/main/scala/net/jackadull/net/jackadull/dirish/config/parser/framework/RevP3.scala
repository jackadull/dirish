package net.jackadull.net.jackadull.dirish.config.parser.framework

import net.jackadull.net.jackadull.dirish.config.parser.framework.RevP3.Matcher

import scala.annotation.tailrec

trait RevP3[A] extends RevPCombinators[A] {
  def apply[S[+_]](src:Src2[S]):S[Unit]<=>S[A]
  def matcher:Matcher
}
object RevP3 {
  def apply(char:Char):Matcher = Matcher.MatchChar(char)

  trait Matcher extends RevP3[Unit] {
    override def apply[S[+_]](src:Src2[S]):S[Unit]<=>S[Unit] = <=>.symmetric(`match`(_, src))
    def `match`[S[+_]](s:S[Unit], src:Src2[S]):S[Unit]
    override def matcher:Matcher = this
  }
  private[framework] object Matcher {
    final case class MatchAlt(alternatives:List[Matcher]) extends Matcher {
      override def `match`[S[+_]](s:S[Unit], src:Src2[S]):S[Unit] = {
        @tailrec def recurse(s0:S[Unit], a:List[Matcher]):S[Unit] = a match {
          case Nil => src.fail(s0, "Unexpected input")
          case first :: rest =>
            val s1 = first.`match`(s0, src)
            if(src.isSuccess(s1)) s1 else recurse(s0, rest)
        }
        recurse(s, alternatives)
      }
    }

    final case class MatchChar(char:Char) extends Matcher {
      override def `match`[S[+_]](s:S[Unit], src:Src2[S]):S[Unit] = src(s, char)
    }

    final case class MatchOpt(optional:Matcher) extends Matcher {
      override def `match`[S[+_]](s:S[Unit], src:Src2[S]):S[Unit] = {
        val s0 = optional.`match`(s, src)
        if(src.isSuccess(s0)) s0 else s
      }
    }

    final case class MatchSeq(elements:List[Matcher]) extends Matcher {
      override def `match`[S[+_]](s:S[Unit], src:Src2[S]):S[Unit] = {
        @tailrec def recurse(s0:S[Unit], e:List[Matcher]):S[Unit] = e match {
          case Nil => s0
          case first :: rest =>
            val s1 = first.`match`(s0, src)
            if(src.isSuccess(s1)) recurse(s1, rest) else s1
        }
        recurse(s, elements)
      }
    }
  }

  private[framework] final case class Mapped[A,B](p:RevP3[A], f:A<=>B) extends RevP3[B] {
    override def apply[S[+_]](src:Src2[S]):S[Unit]<=>S[B] = p(src).andThen(<=>(src.map(_)(f.to), src.map(_)(f.from)))
    override def matcher:Matcher = p.matcher
  }

  private[framework] final case class ParseOpt[A](optional:RevP3[A]) extends RevP3[Option[A]] {
    override def apply[S[+_]](src:Src2[S]):S[Unit]<=>S[Option[A]] = {
      // TODO fail if the given s is failed
      optional(src).andThen(<=>(
        to = s0 => if(src.isSuccess(s0)) src.map(s0)(Some(_)) else ,
        from = ???
      ))
    }
    override def matcher:Matcher = optional.matcher.?
  }

  private[framework] final case class ParseTuple2[A,B](l:RevP3[A], r:RevP3[B]) extends RevP3[(A,B)] {
    override def apply[S[+_]](src:Src2[S]):S[Unit]<=>S[(A,B)] = {
      val (ls, rs) = (l(src), r(src))
      <=>( // TODO fail fast
        to = {s0 =>
          val s1:S[A] = ls.to(s0)
          val s2:S[B] = rs.to(src.unit(s1))
          src.flatMap(s2) {b => src.map(s1) {a => (a, b)}}
        },
        from = {s0 =>
          val s1:S[Unit] = ls.from(src.map(s0) {_._1})
          val s2:S[Unit] = rs.from(src.copy(src.map(s0) {_._2}, s1))
          s2
        }
      )
    }

    override def matcher:Matcher = l.matcher ~ r.matcher
  }
}
