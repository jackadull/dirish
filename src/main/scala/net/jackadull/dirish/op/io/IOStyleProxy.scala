package net.jackadull.dirish.op.io

import java.nio.charset.Charset

import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

trait IOStyleProxy[V[+_,+_]] extends IOStyle[V] {
  protected def ioStyle:IOStyle[V]

  def createDirectories(path:AbsolutePathSpec):V[Unit,CreateDirectoryError] = ioStyle createDirectories path
  def createDirectory(path:AbsolutePathSpec):V[Unit,CreateDirectoryError] = ioStyle createDirectory path
  def createLockFile(path:AbsolutePathSpec):V[Unit,CreateLockFileError] = ioStyle createLockFile path
  def deleteFile(path:AbsolutePathSpec):V[Unit,DeleteFileError] = ioStyle deleteFile path
  def fileKindInfo(path:AbsolutePathSpec):V[FileKind,FileKindInfoError] = ioStyle fileKindInfo path
  def listDirectory(path:AbsolutePathSpec):V[Set[AbsolutePathSpec],ListDirectoryError] = ioStyle listDirectory path
  def moveFile(source:AbsolutePathSpec, target:AbsolutePathSpec):V[Unit,MoveFileError] = ioStyle moveFile (source, target)
  def moveFileToTrash(path:AbsolutePathSpec):V[Unit,MoveToTrashError] = ioStyle moveFileToTrash path
  def readFileAsString(path:AbsolutePathSpec, charset:Charset):V[String,ReadFileAsStringError] = ioStyle readFileAsString (path, charset)
  def saveStringToFile(path:AbsolutePathSpec, str:String, charset:Charset):V[Unit,SaveStringToFileError] = ioStyle saveStringToFile (path, str, charset)
}
