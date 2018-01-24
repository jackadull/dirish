package net.jackadull.dirish.io

import net.jackadull.dirish.path.AbsolutePathSpec

sealed trait FileInfo {
  def exists:Boolean
  def isDirectory:Boolean
  def isPlainFile:Boolean
  def path:AbsolutePathSpec
}
sealed trait ExistingFileInfo extends FileInfo {def exists = true}
final case class NonExistingFileInfo(path:AbsolutePathSpec) extends FileInfo {
  def exists = false
  def isDirectory = false
  def isPlainFile = false
}

final case class DirectoryFileInfo(path:AbsolutePathSpec) extends ExistingFileInfo {
  def isDirectory = true
  def isPlainFile = false
}
final case class PlainFileInfo(path:AbsolutePathSpec) extends ExistingFileInfo {
  def isDirectory = false
  def isPlainFile = true
}
