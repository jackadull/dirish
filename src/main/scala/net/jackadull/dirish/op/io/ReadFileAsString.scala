package net.jackadull.dirish.op.io

import java.nio.charset.Charset

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class ReadFileAsString(path:AbsolutePathSpec, charset:Charset) extends Op[String,ReadFileAsStringError,IOStyle] {
  def instantiateIn[V[+_,+_]](style:IOStyle[V]):V[String,ReadFileAsStringError] = style readFileAsString (path, charset)
}
