package net.jackadull.net.jackadull.dirish.config.parser.framework

trait Src2[S[+_]] {
  def apply(s:S[Unit], char:Char):S[Unit]
}
