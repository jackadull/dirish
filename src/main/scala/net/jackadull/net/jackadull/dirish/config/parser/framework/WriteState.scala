package net.jackadull.net.jackadull.dirish.config.parser.framework

import net.jackadull.net.jackadull.dirish.config.parser.framework.RevP.Matcher

sealed trait WriteState[+Repr<:WriteState[Repr]] {
  this:Repr=>
  def appending(char:Char):Repr
  def appending(string:String):Repr

  def appending(matcher:Matcher):Repr = matcher.generate(this)
}
