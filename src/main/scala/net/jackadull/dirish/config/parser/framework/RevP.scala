package net.jackadull.dirish.config.parser.framework

import net.jackadull.dirish.config.parser.framework.RevP.Matcher

import scala.annotation.tailrec
import scala.language.implicitConversions

trait RevP[A] extends RevPCombinators[A] {
  def apply[S[+_]](src:Src[S]):S[Unit]<=>S[A]
  def matcher:Matcher
}
object RevP {
  implicit def apply(char:Char):Matcher = Matcher.MatchChar(char)
  implicit def apply(string:String):Matcher = Matcher.MatchString(string)

  trait Matcher extends RevP[Unit] {
    override def apply[S[+_]](src:Src[S]):S[Unit]<=>S[Unit] = <=>.symmetric(`match`(_, src))
    def `match`[S[+_]](s:S[Unit], src:Src[S]):S[Unit]
    override def matcher:Matcher = this
  }
  private[framework] object Matcher {
    final case class MatchAlt(alternatives:List[Matcher]) extends Matcher {
      override def `match`[S[+_]](s:S[Unit], src:Src[S]):S[Unit] = {
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
      override def `match`[S[+_]](s:S[Unit], src:Src[S]):S[Unit] = src(s, char)
    }

    final case class MatchOpt(optional:Matcher) extends Matcher {
      override def `match`[S[+_]](s:S[Unit], src:Src[S]):S[Unit] = {
        val s0 = optional.`match`(s, src)
        if(src.isSuccess(s0)) s0 else s
      }
    }

    final case class MatchRep(repeated:Matcher) extends Matcher {
      override def `match`[S[+_]](s:S[Unit], src:Src[S]):S[Unit] = {
        @tailrec def recurse(s:S[Unit]):S[Unit] = {
          val s0 = repeated.`match`(s, src)
          if(src.isSuccess(s0)) recurse(s0) else s
        }
        recurse(s)
      }
    }

    final case class MatchSeq(elements:List[Matcher]) extends Matcher {
      override def `match`[S[+_]](s:S[Unit], src:Src[S]):S[Unit] = {
        @tailrec def recurse(s0:S[Unit], e:List[Matcher]):S[Unit] = e match {
          case Nil => s0
          case first :: rest =>
            val s1 = first.`match`(s0, src)
            if(src.isSuccess(s1)) recurse(s1, rest) else s1
        }
        recurse(s, elements)
      }
    }

    final case class MatchString(string:String) extends Matcher {
      override def `match`[S[+_]](s:S[Unit], src:Src[S]):S[Unit] = src(s, string)
    }
  }

  private[framework] final case class ExtractTuple2_1[A](p:RevP[(A,Unit)]) extends RevP[A] {
    override def apply[S[+_]](src:Src[S]):S[Unit]<=>S[A] = {
      val ps = p(src)
      <=>(
        to = {s =>
          val s0:S[(A,Unit)] = ps.to(s)
          src.flatMap(s0)(t => src.set(s0, t._1))
        },
        from = sa => ps.from(src.map(sa)(a => (a, ())))
      )
    }
    override def matcher:Matcher = p.matcher
  }

  private[framework] final case class ExtractTuple2_2[A](p:RevP[(Unit,A)]) extends RevP[A] {
    override def apply[S[+_]](src:Src[S]):S[Unit]<=>S[A] = p.map(<=>.tuple2_2(()))(src)
    override def matcher:Matcher = p.matcher
  }

  private[framework] final case class Mapped[A,B](p:RevP[A], f:A<=>B) extends RevP[B] {
    override def apply[S[+_]](src:Src[S]):S[Unit]<=>S[B] = p(src).andThen(<=>(src.map(_)(f.to), src.map(_)(f.from)))
    override def matcher:Matcher = p.matcher
  }

  private[framework] final case class ParseOpt[A](optional:RevP[A]) extends RevP[Option[A]] {
    override def apply[S[+_]](src:Src[S]):S[Unit]<=>S[Option[A]] = {
      val os = optional(src)
      <=>(
        to = {s0 =>
          val s1 = os.to(s0)
          if(src.isSuccess(s1)) src.map(s1)(Some(_)) else src.set(s0, None)
        },
        from = s => src.flatMap(s) {
          case None => src.set(s, ())
          case Some(a) => os.from(src.set(s, a))
        }
      )
    }
    override def matcher:Matcher = optional.matcher.?
  }

  private[framework] final case class ParseRep[A](element:RevP[A]) extends RevP[Seq[A]] {
    override def apply[S[+_]](src:Src[S]):S[Unit]<=>S[Seq[A]] = {
      val es = element(src)
      <=>(
        to = {s =>
          @tailrec def recurse(s0:S[List[A]]):S[Seq[A]] = {
            val s1 = es.to(src.set(s0, ()))
            if(src.isSuccess(s1)) recurse(src.flatMap(s1)(a => src.map(s0)(a :: _)))
            else src.map(s0)(_.toVector.reverse)
          }
          recurse(src.set(s, Nil))
        },
        from = {s =>
          @tailrec def recurse(s0:S[Any], a:List[A]):S[Unit] = a match {
            case Nil => src.set(s0, ())
            case first :: rest =>
              val s1 = es.from(src.set(s0, first))
              if(src.isSuccess(s1)) recurse(s1, rest) else src.set(s1, ())
          }
          src.flatMap(s)(as => recurse(s, as.toList))
        }
      )
    }
    override def matcher:Matcher = element.matcher.:*
  }

  private[framework] final case class ParseRep1[A](element:RevP[A]) extends RevP[Seq[A]] {
    override def apply[S[+_]](src:Src[S]):S[Unit]<=>S[Seq[A]] = {
      val es = element(src)
      <=>(
        to = {s =>
          @tailrec def recurse(s0:S[List[A]]):S[Seq[A]] = {
            val s1 = es.to(src.set(s0, ()))
            if(src.isSuccess(s1)) recurse(src.flatMap(s1)(a => src.map(s0)(a :: _)))
            else src.map(s0)(_.toVector.reverse)
          }
          val results = recurse(src.set(s, Nil))
          src.flatMap(results)(as => if(as.isEmpty) src.fail(results, "Expected at least one") else results)
        },
        from = {s =>
          @tailrec def recurse(s0:S[Any], a:List[A]):S[Unit] = a match {
            case Nil => src.set(s0, ())
            case first :: rest =>
              val s1 = es.from(src.set(s0, first))
              if(src.isSuccess(s1)) recurse(s1, rest) else src.set(s1, ())
          }
          src.flatMap(s)(as =>
            if(as.isEmpty) src.set(src.fail(s, "Expected at least one"), ()) else recurse(s, as.toList)
          )
        }
      )
    }
    override def matcher:Matcher = element.matcher.:+
  }

  private[framework] final case class ParseTuple2[A,B](l:RevP[A], r:RevP[B]) extends RevP[(A,B)] {
    override def apply[S[+_]](src:Src[S]):S[Unit]<=>S [(A,B)] = {
      val (ls, rs) = (l(src), r(src))
      <=>( // TODO fail fast
        to = {s0 =>
          val s1:S[A] = ls.to(s0)
          val s2:S[B] = rs.to(src.set(s1, ()))
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
