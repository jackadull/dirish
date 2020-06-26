package net.jackadull.dirish.config.parser.framework

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RevPSpec extends AnyFreeSpec with Matchers {
  "A simple character parser" - {
    val p = RevP('a')
    "successfully parses the character" in {
      MemorySrc.parse("a", p) should matchPattern {case _:MemorySrc.ReadState.Success[Any] =>}
      MemorySrc.parse("abcd", p) should matchPattern {case _:MemorySrc.ReadState.Success[Any] =>}
    }
    "fail on invalid input" in {
      MemorySrc.parse("b", p) should matchPattern {case _:MemorySrc.ReadState.Failure =>}
      MemorySrc.parse("bcde", p) should matchPattern {case _:MemorySrc.ReadState.Failure =>}
      MemorySrc.parse("", p) should matchPattern {case _:MemorySrc.ReadState.Failure =>}
    }
  }
}
