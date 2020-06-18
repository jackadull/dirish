package net.jackadull.net.jackadull.dirish.config.parser.framework

import scala.annotation.tailrec

trait RevP2[A] {
  def apply[S[+_]](src:Src[S]):S[Any]<=>S[A]
}
object RevP2 {
  trait Match extends RevP2[Any] {
    override def apply[S[+_]](src:Src[S]):S[Any]<=>S[Any] = <=>.symmetric(`match`(_, src))
    def `match`[S[+_]](s:S[Any], src:Src[S]):S[Any]
  }

  private final case class MatchChar(char:Char) extends Match {
    override def `match`[S[+_]](s:S[Any], src:Src[S]):S[Any] = src(s, char)
  }

  private final case class MatchOpt(optional:Match) extends Match {
    override def `match`[S[+_]](s:S[Any], src:Src[S]):S[Any] = src.orElse(optional.`match`(s, src), s)
  }

  private final case class MatchSeq(elements:List[Match]) extends Match {
    override def `match`[S[+_]](s:S[Any], src:Src[S]):S[Any] = {
      @tailrec def recurse(s0:S[Any], e:List[Match]):S[Any] = e match {
        case fst :: rst =>
          val s1 = fst.`match`(s0, src)
          if(src.isSuccess(s1)) recurse(s1, rst) else s1
        case Nil => s0
      }
      if(src.isSuccess(s)) recurse(s, elements) else s
    }
  }

  private final case class RevPSurround[A](left:Match, mid:RevP2[A], right:Match) extends RevP2[A] {
    override def apply[S[+_]](src:Src[S]):S[Any]<=>S[A] = {
      val sMid:S[Any]<=>S[A] = left(src).andThen(mid(src))
      val sRight:S[Any]<=>S[Any] = right(src)
      val carryRight:S[A]<=>S[A] = <=>( // TODO this looks suspiciously like something generic
        r0 => sRight.to.andThen(src.carry(_, r0))(r0),
        w0 => sRight.from.andThen(src.carry(_, w0))(w0)
      )
      sMid.andThen(carryRight)
    }
  }
}
