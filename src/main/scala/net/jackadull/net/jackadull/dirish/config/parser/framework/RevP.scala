package net.jackadull.net.jackadull.dirish.config.parser.framework

import net.jackadull.net.jackadull.dirish.config.parser.framework.ParseResult.{ParseError, ParseSuccess}
import net.jackadull.net.jackadull.dirish.config.parser.framework.RevP.Matcher

import scala.annotation.tailrec

/** Reversible, partially isomorphic, composable parser. Can parse input to the target type (or return an error), and
 * can also generate the source text, given an instance of the target type. */
trait RevP[A] {
  def generate[W<:WriteState[W]](instance:A, write:W):W
  def matcher:Matcher
  def parse(read:ReadState):ParseResult[A]
  def ~(that:RevP[_]):Matcher = RevP.~(matcher, that.matcher)

  def map[A2](iso:A<=>A2):RevP[A2] = RevP.Mapped(this, iso)
  def ~>[A2](that:RevP[A2]):RevP[A2] = RevP.~>(matcher, that)
  def <~(that:RevP[_]):RevP[A] = RevP.<~(this, that.matcher)
  def |(that:RevP[_]):Matcher = RevP.|(matcher, that.matcher)
  def ? :Matcher = RevP.?(matcher)
  def ?< :RevP[Option[A]] = RevP.?<(this)
}
object RevP {
  def apply(char:Char):Matcher = OneChar(char)
  def apply(string:String):Matcher = OneString(string)
  def empty:Matcher = Empty
  def eof:Matcher = EOF

  private def ~(a:Matcher, b:Matcher):Matcher = (a, b) match {
    case (Empty, _) => b
    case (_, Empty) => a
    case (SeqMatcher(e1), SeqMatcher(e2)) => SeqMatcher(e1 ++ e2)
    case (SeqMatcher(e1), _) => SeqMatcher(e1 :+ b)
    case (_, SeqMatcher(e2)) => SeqMatcher(a +: e2)
    case _ => SeqMatcher(List(a, b))
  }

  private def ~>[A](a:Matcher, b:RevP[A]):RevP[A] = (a, b) match {
    case (Empty, _) => b
    case (_, SeqSurround(l, m, r)) => SeqSurround(a~l, m, r)
    case _ => SeqSurround(a, b, empty)
  }

  private def <~[A](a:RevP[A], b:Matcher):RevP[A] = (a, b) match {
    case (_, Empty) => a
    case (SeqSurround(l, m, r), _) => SeqSurround(l, m, r~b)
    case _ => SeqSurround(empty, a, b)
  }

  private def |(a:Matcher, b:Matcher):Matcher = (a, b) match {
    case (AltMatcher(as), AltMatcher(bs)) => AltMatcher(as ++ bs)
    case (AltMatcher(as), _) => AltMatcher(as :+ b)
    case (_, AltMatcher(bs)) => AltMatcher(a +: bs)
    case _ => AltMatcher(List(a, b))
  }

  private def ?(a:Matcher):Matcher = a match {
    case _:OptMatcher => a
    case _ => OptMatcher(a)
  }

  private def ?<[A](a:RevP[A]):RevP[Option[A]] = OptRevP(a)

  trait Matcher extends RevP[Unit] {
    def generate[W<:WriteState[W]](write:W):W

    override def generate[W<:WriteState[W]](instance:Unit, write:W):W = generate(write)
    override def matcher:Matcher = this
  }

  private final case class AltMatcher(alts:List[Matcher]) extends Matcher {
    override def generate[W<:WriteState[W]](write:W):W = alts.headOption.map(_.generate(write)).getOrElse(write)
    override def parse(read:ReadState):ParseResult[Unit] = {
      @tailrec def recurse(r:ReadState, a:List[Matcher]):ParseResult[Unit] = a match { // TODO combine failures
        case fst :: rst => fst.parse(r) match {
          case success:ParseSuccess[Unit] => success
          case error:ParseError => error
          case _ => recurse(r, rst)
        }
        case Nil => r.failure()
      }
      recurse(read, alts)
    }
  }

  private object Empty extends Matcher {
    override def generate[W<:WriteState[W]](write:W):W = write
    override def parse(read:ReadState):ParseResult[Unit] = read.success(())
  }

  private object EOF extends Matcher {
    override def generate[W<:WriteState[W]](write:W):W = write
    override def parse(read:ReadState):ParseResult[Unit] = read.expectingEOF
  }

  private final case class Mapped[A,A2](mapped:RevP[A], iso:A<=>A2) extends RevP[A2] {
    override def generate[W<:WriteState[W]](instance:A2, write:W):W = mapped.generate(iso.from(instance), write)
    override def matcher:Matcher = mapped.matcher
    override def parse(read:ReadState):ParseResult[A2] = mapped.parse(read).mapResult(iso.to)
  }

  private final case class OneChar(char:Char) extends Matcher {
    override def generate[W<:WriteState[W]](write:W):W = write.appending(char)
    override def parse(read:ReadState):ParseResult[Unit] = read.expecting(char)
  }

  private final case class OneString(string:String) extends Matcher {
    override def generate[W<:WriteState[W]](write:W):W = write.appending(string)
    override def parse(read:ReadState):ParseResult[Unit] = read.expecting(string)
  }

  private final case class OptMatcher(optional:Matcher) extends Matcher {
    override def generate[W<:WriteState[W]](write:W):W = write
    override def parse(read:ReadState):ParseResult[Unit] = optional.parse(read).orElse(read.success(()))
  }

  private final case class OptRevP[A](optional:RevP[A]) extends RevP[Option[A]] {
    override def generate[W<:WriteState[W]](instance:Option[A], write:W):W =
      instance.map(optional.generate(_, write)).getOrElse(write)
    override def matcher:Matcher = optional.matcher.?
    override def parse(read:ReadState):ParseResult[Option[A]] =
      optional.parse(read).mapResult(Some(_)).orElse(read.success(None))
  }

  private final case class SeqMatcher(elements:List[Matcher]) extends Matcher {
    override def generate[W<:WriteState[W]](write:W):W = elements.foldLeft(write)(_.appending(_))
    override def parse(read:ReadState):ParseResult[Unit] = {
      @tailrec def recurse(r:ReadState, e:List[Matcher]):ParseResult[Unit] = e match {
        case fst :: rst => fst.parse(r) match {
          case ParseSuccess(_, r2) => recurse(r2, rst)
          case noSuccess => noSuccess
        }
        case Nil => r.success(())
      }
      recurse(read, elements)
    }
  }

  private final case class SeqSurround[A](left:Matcher, mid:RevP[A], right:Matcher) extends RevP[A] {
    override def generate[W<:WriteState[W]](instance:A, write:W):W =
      right.generate(mid.generate(instance, left.generate(write)))
    override def matcher:Matcher = left ~ mid ~ right
    override def parse(read:ReadState):ParseResult[A] =
      left.parse(read).flatMap((_, r2) => mid.parse(r2).flatMap((result, r3) => right.parse(r3).mapResult(_ => result)))
  }
}
