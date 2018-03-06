package net.jackadull.dirish.op.io

import java.nio.charset.Charset

import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

trait IOStyle[V[+_,+_]] {
  def createDirectories(path:AbsolutePathSpec):V[Unit,CreateDirectoryError]
  def createDirectory(path:AbsolutePathSpec):V[Unit,CreateDirectoryError]
  def createLockFile(path:AbsolutePathSpec):V[Unit,CreateLockFileError]
  def deleteFile(path:AbsolutePathSpec):V[Unit,DeleteFileError]
  def fileKindInfo(path:AbsolutePathSpec):V[FileKind,FileKindInfoError]
  def listDirectory(path:AbsolutePathSpec):V[Set[AbsolutePathSpec],ListDirectoryError]
  def moveFile(source:AbsolutePathSpec, target:AbsolutePathSpec):V[Unit,MoveFileError]
  def moveFileToTrash(path:AbsolutePathSpec):V[Unit,MoveToTrashError]
  def readFileAsString(path:AbsolutePathSpec, charset:Charset):V[String,ReadFileAsStringError]
  def saveStringToFile(path:AbsolutePathSpec, str:String, charset:Charset):V[Unit,SaveStringToFileError]
}
