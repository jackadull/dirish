package net.jackadull.net.jackadull.dirish.config.parser.framework

import net.jackadull.net.jackadull.dirish.config.parser.framework.RevP.Matcher

sealed trait WriteState {
  def appending(char:Char):WriteState
  def appending(string:String):WriteState

  def appending(matcher:Matcher):WriteState = matcher.generate(this)
}
