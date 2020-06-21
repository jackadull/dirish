package net.jackadull.net.jackadull.dirish.config.parser.framework

import scala.annotation.tailrec

trait RevP3[A] {
  def apply[S[+_]](src:Src2[S]):S[Unit]<=>S[A]
}
object RevP3 {
  trait Matcher extends RevP3[Unit] {
    override def apply[S[+_]](src:Src2[S]):S[Unit]<=>S[Unit] = <=>.symmetric(`match`(_, src))
    def `match`[S[+_]](s:S[Unit], src:Src2[S]):S[Unit]
  }

  private final case class ASeq[A](elements:List[RevP3[A]]) extends RevP3[Seq[A]] {
    override def apply[S[+_]](src:Src2[S]):S[Unit]<=>S[Seq[A]] = {
      @tailrec def recurse(e:List[RevP3[A]])
    }
  }

  private final case class MatchOneChar(char:Char) extends Matcher {
    override def `match`[S[+_]](s:S[Unit], src:Src2[S]):S[Unit] = src(s, char)
  }
}
