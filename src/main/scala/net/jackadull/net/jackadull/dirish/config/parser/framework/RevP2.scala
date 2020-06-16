package net.jackadull.net.jackadull.dirish.config.parser.framework

import scala.annotation.tailrec

trait RevP2[A] {
  def apply[S[+_]](src:Src[S]):S[Nothing]<=>S[A]
}
object RevP2 {
  trait Match extends RevP2[Nothing] {
    override def apply[S[+_]](src:Src[S]):S[Nothing]<=>S[Nothing] = <=>.symmetric(`match`(_, src))
    def `match`[S[+_]](s:S[Nothing], src:Src[S]):S[Nothing]
  }

  private final case class MatchChar(char:Char) extends Match {
    override def `match`[S[+_]](s:S[Nothing], src:Src[S]):S[Nothing] = src(s, char)
  }

  private final case class MatchOpt(optional:Match) extends Match {
    override def `match`[S[+_]](s:S[Nothing], src:Src[S]):S[Nothing] = src.orElse(optional.`match`(s, src), s)
  }

  private final case class MatchSeq(elements:List[Match]) extends Match {
    override def `match`[S[+_]](s:S[Nothing], src:Src[S]):S[Nothing] = {
      @tailrec def recurse(s0:S[Nothing], e:List[Match]):S[Nothing] = e match {
        case fst :: rst =>
          val s1 = fst.`match`(s0, src)
          if(src.isSuccess(s1)) recurse(s1, rst) else s1
        case Nil => s0
      }
      if(src.isSuccess(s)) recurse(s, elements) else s
    }
  }

  private final case class RevPSurround[A](left:Match, mid:RevP2[A], right:Match) extends RevP2[A] {
    override def apply[S[+_]](src:Src[S]):S[Nothing]<=>S[A] = {
      val sMid:S[Nothing]<=>S[A] = left(src).andThen(mid(src))
      val sRight:S[Nothing]<=>S[Nothing] = right(src)
      val carryRight:S[A]<=>S[A] = <=>( // TODO this looks suspiciously like something generic
        r0 => sRight.to.andThen(src.carry(_, r0))(src.empty(r0)),
        w0 => sRight.from.andThen(src.carry(_, w0))(src.empty(w0))
      )
      sMid.andThen(carryRight)
    }
  }
}
