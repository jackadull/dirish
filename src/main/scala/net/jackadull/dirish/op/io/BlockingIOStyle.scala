package net.jackadull.dirish.op.io

import java.awt.{Desktop, HeadlessException}
import java.io.{IOException, OutputStreamWriter}
import java.nio.charset.Charset
import java.nio.file._

import net.jackadull.dirish.op.GenericThrowableError
import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, CombinatorStyle, EitherV}
import net.jackadull.dirish.op.util.UsingCombinator
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec, UserHomePathSpec}

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet
import scala.language.{higherKinds, postfixOps, reflectiveCalls}

class BlockingIOStyle[V[+_,+_]](protected val combinatorStyle:CombinatorStyle[V], userHome:String, fs:FileSystem=FileSystems getDefault)
extends IOStyle[V] with UsingCombinator[V] {
  def createDirectories(path:AbsolutePathSpec):V[Unit,CreateDirectoryError] =
    vtry({Files createDirectories toPath(path)}, {
      case e:FileAlreadyExistsException ⇒ FileAlreadyExists(e getFile, s"Cannot create directories at $path")
      case t ⇒ GenericThrowableError(s"Cannot create directories at $path", t)
    })

  def createDirectory(path:AbsolutePathSpec):V[Unit,CreateDirectoryError] =
    vtry({Files createDirectory toPath(path)}, {
      case e:FileAlreadyExistsException ⇒ FileAlreadyExists(e getFile, s"Cannot create directory at $path")
      case t ⇒ GenericThrowableError(s"Cannot create directory at $path", t)
    })

  def createLockFile(path:AbsolutePathSpec):V[Unit,CreateLockFileError] =
    vtry({Files createFile toPath(path)}, {
      case e:FileAlreadyExistsException ⇒ FileAlreadyExists(e getFile, s"Cannot create lock file at $path")
      case t ⇒ GenericThrowableError(s"Cannot create lock file at $path", t)
    })

  def deleteFile(path:AbsolutePathSpec):V[Unit,DeleteFileError] =
    vtry({Files delete toPath(path)}, {
      case e:DirectoryNotEmptyException ⇒ DirectoryNotEmpty(s"Cannot delete file at $path", e getFile)
      case e:NoSuchFileException ⇒ NoSuchFile(s"Cannot delete file at $path", e getFile)
      case t ⇒ GenericThrowableError(s"Cannot delete file at $path", t)
    })

  def fileKindInfo(path:AbsolutePathSpec):V[FileKind,FileKindInfoError] =
    vtry ({
      val p = toPath(path)
      if(!(Files exists p)) IsNonExistent
      else if(Files isDirectory p) IsDirectory
      else IsRegularFile
    }, {
      case t ⇒ GenericThrowableError(s"Cannot check the file type of $path", t)
    })

  def listDirectory(path:AbsolutePathSpec):V[Set[AbsolutePathSpec],ListDirectoryError] = {
    def toChild(p:Path):AbsolutePathSpec = path / (p getFileName).toString
    vtry({HashSet(Files.list(toPath(path)).iterator().asScala.map(toChild).toSeq:_*)}, {
      case e:NotDirectoryException ⇒ NotADirectory(s"Cannot list contents of $path", e getFile)
      case t ⇒ GenericThrowableError(s"Cannot list contents of $path", t)
    })
  }

  def moveFile(source:AbsolutePathSpec, target:AbsolutePathSpec):V[Unit,MoveFileError] =
    vtry({Files move (toPath(source), toPath(target))}, {
      case e:FileAlreadyExistsException ⇒ FileAlreadyExists(s"Cannot move file from $source to $target", e getFile)
      case t ⇒ GenericThrowableError(s"Cannot move file from $source to $target", t)
    })

  def moveFileToTrash(path:AbsolutePathSpec):V[Unit,MoveToTrashError] =
    vtry({
      if(!(Desktop.getDesktop moveToTrash (toPath(path) toFile))) throw new IOException("Desktop.moveToTrash returned false")
    }, {
      case _:HeadlessException ⇒ MoveToTrashNotSupported(s"Cannot move $path to trash because move to trash is not supported (headless graphics environment)")
      case t ⇒ GenericThrowableError(s"Cannot move $path to trash", t)
    })


  def readFileAsString(path:AbsolutePathSpec, charset:Charset):V[String,ReadFileAsStringError] =
    vtry({
      val input = Files newInputStream toPath(path)
      try {new String(input readAllBytes, charset)} finally {input close()}
    }, {case t ⇒ GenericThrowableError(s"Cannot read $path", t)})


  def saveStringToFile(path:AbsolutePathSpec, str:String, charset:Charset):V[Unit,SaveStringToFileError] =
    vtry({
      val output = Files newOutputStream toPath(path)
      try {val writer = new OutputStreamWriter(output, charset); writer write str; writer flush()}
      finally {output close()}
    }, {case t ⇒ GenericThrowableError(s"Cannot write $path", t)})

  private val userHomePath:Path = fs getPath userHome
  private def toPath(p:AbsolutePathSpec):Path = p match {
    case UserHomePathSpec ⇒ userHomePath
    case CompositeAbsolutePathSpec(parent, child) ⇒ toPath(parent) resolve (child name)
  }
}
object BlockingIOStyle extends BlockingIOStyle[EitherV](BlockingEitherCombinatorStyle, System getProperty "user.home", FileSystems getDefault)
