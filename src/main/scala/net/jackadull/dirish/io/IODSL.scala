package net.jackadull.dirish.io

import java.nio.charset.Charset

import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.{higherKinds, postfixOps}

object IODSL {
  trait IOOp[+R] {
    def instantiate[I[+_]](io:IO[I]):I[R]

    def expecting[R2>:R](expectation:PartialFunction[R,R2])(implicit ev:GenericIOError⇒R2):IOOp[R2] =
      map {v ⇒ if(expectation isDefinedAt v) expectation(v) else GenericIOError(new RuntimeException(s"Unexpected IO result: $v"))}

    def flatMap[R2](f:R⇒IOOp[R2]):IOOp[R2] = IOFlatMap(this, f)
    def map[R2](f:R⇒R2):IOOp[R2] = IOMap(this, f)
    def pf[R2>:R](pf:PartialFunction[R,R2]):IOOp[R2] = map {v ⇒ if(pf isDefinedAt v) pf(v) else v}
  }

  final case class IOSeq[+R](ops:Seq[IOOp[R]]) extends IOOp[R] {
    def instantiate[I[+_]](io:IO[I]):I[R] = {
      def recurse(o:Seq[I[R]], soFar:I[R]):I[R] = o match {
        case Seq() ⇒ soFar
        case Seq(fst,rst@_*) ⇒
          val soFar2 = io.flatMap(soFar) {
            case err if err.isInstanceOf[IOError] ⇒ io bind err
            case _ ⇒ fst
          }
          recurse(rst, soFar2)
      }
      recurse((ops tail) map {_ instantiate io}, (ops head) instantiate io)
    }
  }

  final case class AddGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String) extends IOOp[AddGitModuleRemoteResult] {
    def instantiate[I[+_]](io:IO[I]):I[AddGitModuleRemoteResult] = io addGitModuleRemote (gitModulePath, remoteName, remoteURI)
  }
  final case class CloneGitModule(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String) extends IOOp[CloneGitModuleResult] {
    def instantiate[I[+_]](io:IO[I]):I[CloneGitModuleResult] = io cloneGitModule (gitModulePath, remoteName, remoteURI)
  }
  final case class CreateDirectory(directoryPath:AbsolutePathSpec) extends IOOp[CreateDirectoryResult] {
    def instantiate[I[+_]](io:IO[I]):I[CreateDirectoryResult] = io createDirectory directoryPath
  }
  final case class CreateLockFile(path:AbsolutePathSpec) extends IOOp[CreateLockFileResult] {
    def instantiate[I[+_]](io:IO[I]):I[CreateLockFileResult] = io createLockFile path
  }
  final case class GetFileInfo(path:AbsolutePathSpec) extends IOOp[GetFileInfoResult] {
    def instantiate[I[+_]](io:IO[I]):I[GetFileInfoResult] = io getFileInfo path
  }
  final case class HasLocalGitChanges(gitModulePath:AbsolutePathSpec) extends IOOp[HasLocalGitChangesResult] {
    def instantiate[I[+_]](io:IO[I]):I[HasLocalGitChangesResult] = io hasLocalGitChanges gitModulePath
  }
  final case class IsDirectoryEmptyEnoughAsMoveTarget(directoryPath:AbsolutePathSpec) extends IOOp[IsDirectoryEmptyEnoughAsMoveTargetResult] {
    def instantiate[I[+_]](io:IO[I]):I[IsDirectoryEmptyEnoughAsMoveTargetResult] = io isDirectoryEmptyEnoughAsMoveTarget directoryPath
  }
  final case class IsDirectoryEmptyEnoughForRemoving(directoryPath:AbsolutePathSpec) extends IOOp[IsDirectoryEmptyEnoughForRemovingResult] {
    def instantiate[I[+_]](io:IO[I]):I[IsDirectoryEmptyEnoughForRemovingResult] = io isDirectoryEmptyEnoughForRemoving directoryPath
  }
  final case class ListDirectoryContents(directoryPath:AbsolutePathSpec) extends IOOp[ListDirectoryContentsResult] {
    def instantiate[I[+_]](io:IO[I]):I[ListDirectoryContentsResult] = io listDirectoryContents directoryPath
  }
  final case class Log(category:LogCategory, message:String, throwableOpt:Option[Throwable]=None) extends IOOp[LogResult] {
    def instantiate[I[+_]](io:IO[I]):I[LogResult] = io log (category, message, throwableOpt)
  }
  final case class MoveFile(sourcePath:AbsolutePathSpec, targetPath:AbsolutePathSpec) extends IOOp[MoveFileResult] {
    def instantiate[I[+_]](io:IO[I]):I[MoveFileResult] = io moveFile (sourcePath, targetPath)
  }
  final case class MoveToTrash(path:AbsolutePathSpec) extends IOOp[MoveToTrashResult] {
    def instantiate[I[+_]](io:IO[I]):I[MoveToTrashResult] = io moveToTrash path
  }
  final case class ParameterValue[A](parameter:IOParameter[A]) extends IOOp[A] {
    def instantiate[I[+_]](io:IO[I]):I[A] = io parameterValue parameter
  }
  final case class ReadFileAsString(path:AbsolutePathSpec, charset:Charset) extends IOOp[ReadFileAsStringResult] {
    def instantiate[I[+_]](io:IO[I]):I[ReadFileAsStringResult] = io readFileAsString (path, charset)
  }
  final case class RemoveFile(path:AbsolutePathSpec) extends IOOp[RemoveFileResult] {
    def instantiate[I[+_]](io:IO[I]):I[RemoveFileResult] = io removeFile path
  }
  final case class RemoveGitModule(gitModulePath:AbsolutePathSpec) extends IOOp[RemoveGitModuleResult] {
    def instantiate[I[+_]](io:IO[I]):I[RemoveGitModuleResult] = io removeGitModule gitModulePath
  }
  final case class RemoveGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String) extends IOOp[RemoveGitModuleRemoteResult] {
    def instantiate[I[+_]](io:IO[I]):I[RemoveGitModuleRemoteResult] = io removeGitModuleRemote (gitModulePath, remoteName)
  }
  final case class SaveStringToFile(path:AbsolutePathSpec, contents:String, charset:Charset) extends IOOp[SaveStringToFileResult] {
    def instantiate[I[+_]](io:IO[I]):I[SaveStringToFileResult] = io saveStringToFile (path, contents, charset)
  }

  final case class IOAggregate[A,B](as:Traversable[IOOp[A]], z:B, seqop:(B,A)⇒B, combop:(B,B)⇒B) extends IOOp[B] {
    def instantiate[I[+_]](io:IO[I]):I[B] = io.aggregate(as map {_ instantiate io})(z)(seqop, combop)
  }
  final case class IOBind[+R](v:R) extends IOOp[R] {
    def instantiate[I[+_]](io:IO[I]):I[R] = io bind v
  }
  final case class IOMap[R,+B](a:IOOp[R], f:R⇒B) extends IOOp[B] {
    def instantiate[I[+_]](io:IO[I]):I[B] = io.map(a instantiate io)(f)
  }
  final case class IOFlatMap[R,+B](a:IOOp[R], f:R⇒IOOp[B]) extends IOOp[B] {
    def instantiate[I[+_]](io:IO[I]):I[B] = io.flatMap(a instantiate io)(f andThen {_.instantiate(io)})
  }
}
