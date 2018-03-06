package net.jackadull.dirish.op.io

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

/** Gives general information on whether the file is a regular file, a directory, or does not exist. */
final case class FileKindInfo(path:AbsolutePathSpec) extends Op[FileKind,FileKindInfoError,IOStyle] {
  def instantiateIn[V[+_,+_]](style:IOStyle[V]):V[FileKind,FileKindInfoError] = style fileKindInfo path
}

sealed trait FileKind
object IsDirectory extends FileKind {override def toString = "IsDirectory"}
object IsNonExistent extends FileKind {override def toString = "IsNonExistent"}
object IsRegularFile extends FileKind {override def toString = "IsRegularFile"}
