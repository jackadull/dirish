package net.jackadull.dirish.op.io

import net.jackadull.dirish.op.OpError

trait IOError extends OpError

sealed trait CreateDirectoryError extends IOError
sealed trait CreateLockFileError extends IOError
sealed trait DeleteFileError extends IOError
sealed trait FileKindInfoError extends IOError
sealed trait ListDirectoryError extends IOError
sealed trait MoveFileError extends IOError
sealed trait MoveToTrashError extends IOError
sealed trait ReadFileAsStringError extends IOError
sealed trait SaveStringToFileError extends IOError

trait GenericIOError extends CreateDirectoryError with CreateLockFileError with DeleteFileError with FileKindInfoError with IOError with ListDirectoryError with MoveFileError with MoveToTrashError with ReadFileAsStringError with SaveStringToFileError

final case class DirectoryNotEmpty(msg:String, path:String) extends DeleteFileError
final case class FileAlreadyExists(msg:String, path:String) extends CreateLockFileError with CreateDirectoryError with MoveFileError
final case class MoveToTrashNotSupported(msg:String) extends MoveToTrashError
final case class NoSuchFile(msg:String, path:String) extends DeleteFileError with ListDirectoryError with MoveFileError with MoveToTrashError with ReadFileAsStringError
final case class NotADirectory(msg:String, path:String) extends ListDirectoryError
