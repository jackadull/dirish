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
      recurse(s, elements)
    }
  }

  private final case class RevPSurround[A](left:Match, mid:RevP2[A], right:Match) extends RevP2[A] {
    override def apply[S[+_]](src:Src[S]):S[Nothing]<=>S[A] =
      mid(src) // TODO implement l and r
  }
}
