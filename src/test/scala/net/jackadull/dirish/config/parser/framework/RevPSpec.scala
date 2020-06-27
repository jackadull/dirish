package net.jackadull.dirish.config.parser.framework

import net.jackadull.dirish.config.parser.framework.MemorySrc.{ReadState, WriteState}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RevPSpec extends AnyFreeSpec with Matchers {
  "A simple character parser" - {
    val p = RevP('a')
    "successfully parses the character" in {
      MemorySrc.parse("a", p) should matchPattern {case _:MemorySrc.ReadState.Success[Any] =>}
      MemorySrc.parse("abcd", p) should matchPattern {case _:MemorySrc.ReadState.Success[Any] =>}
    }
    "fails on invalid input" in {
      MemorySrc.parse("b", p) should matchPattern {case _:MemorySrc.ReadState.Failure =>}
      MemorySrc.parse("bcde", p) should matchPattern {case _:MemorySrc.ReadState.Failure =>}
      MemorySrc.parse("", p) should matchPattern {case _:MemorySrc.ReadState.Failure =>}
    }
    "writes the source correctly" in {
      MemorySrc.write((), p) match {
        case s:WriteState.Success[Unit] => s.toString should be ("a")
        case unexpected => fail(s"Unexpected write result: $unexpected")
      }
    }
  }
  "Combining basic parsers works for" - {
    "concatenation" in {
      val p:RevP[Unit] = RevP('a') ~ RevP('b')
      MemorySrc.parse("ab", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.write((), p) match {
        case f:WriteState.Failure => fail(s"Failed writing: $f")
        case s:WriteState.Success[Unit] => s.toString should be ("ab")
      }
    }
    "repetition" in {
      val p:RevP[Unit] = RevP('a').:+ ~ RevP('b')
      MemorySrc.parse("ab", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.parse("aab", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.parse("aaaaaaab", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.write((), p) match {
        case f:WriteState.Failure => fail(s"Failed writing: $f")
        case s:WriteState.Success[Unit] => s.toString should be ("ab")
      }
    }
    "optionality" in {
      val p:RevP[Unit] = RevP('a').? ~ RevP('b')
      MemorySrc.parse("b", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.parse("ab", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.write((), p) match {
        case f:WriteState.Failure => fail(s"Failed writing: $f")
        case s:WriteState.Success[Unit] => s.toString should be ("b")
      }
    }
    "optionality and repetition" in {
      val p:RevP[Option[Unit]] = (RevP('a') <~ RevP('b').:+).?< <~ RevP('c')
      MemorySrc.parse("c", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.parse("abc", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.parse("abbc", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.parse("abbbc", p) should matchPattern {case _:ReadState.Success[Any] =>}
      MemorySrc.write(None, p) match {
        case f:WriteState.Failure => fail(s"Failed writing: $f")
        case s:WriteState.Success[Unit] => s.toString should be ("c")
      }
      MemorySrc.write(Some(()), p) match {
        case f:WriteState.Failure => fail(s"Failed writing: $f")
        case s:WriteState.Success[Unit] => s.toString should be ("abc")
      }
    }
  }
  "A more complex parser" - {
    final case class Greeting(adjective:Option[String])
    val p:RevP[Greeting] = {
      import RevP._
      ("Hello," ~ ' '.:+ ~> ("wonderful" <~ ' '.:+).?< <~ "world!").map(<=>(
        to = {
          case None => Greeting(None)
          case Some(_) => Greeting(Some("wonderful"))
        },
        from = {
          case Greeting(None) => None
          case Greeting(Some(_)) => Some(())
        }
      ))
    }
    "parses the simple input case" in {
      MemorySrc.parse("Hello, world!", p) should matchPattern
        {case ReadState.Success(Greeting(None), _, _) =>}
    }
    "parses a more complex input case" in {
      MemorySrc.parse("Hello,  wonderful     world!", p) should matchPattern
        {case ReadState.Success(Greeting(Some("wonderful")), _, _) =>}
    }
    "writes the source for the simple case" in {
      MemorySrc.write(Greeting(None), p) match {
        case s:WriteState.Success[Unit] => s.toString should be ("Hello, world!")
        case unexpected => fail(s"Unexpected write result: $unexpected")
      }
    }
    "writes the source for the more complex case" in {
      MemorySrc.write(Greeting(Some("wonderful")), p) match {
        case s:WriteState.Success[Unit] => s.toString should be ("Hello, wonderful world!")
        case unexpected => fail(s"Unexpected write result: $unexpected")
      }
    }
  }
}
