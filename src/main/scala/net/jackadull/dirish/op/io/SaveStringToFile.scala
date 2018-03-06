package net.jackadull.dirish.op.io

import java.nio.charset.Charset

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class SaveStringToFile(path:AbsolutePathSpec, string:String, charset:Charset) extends Op[Unit,SaveStringToFileError,IOStyle] {
  def instantiateIn[V[+_,+_]](style:IOStyle[V]):V[Unit,SaveStringToFileError] =
    style saveStringToFile (path, string, charset)
}
