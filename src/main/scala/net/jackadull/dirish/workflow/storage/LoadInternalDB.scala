package net.jackadull.dirish.workflow.storage

import java.nio.charset.StandardCharsets.UTF_8

import net.jackadull.dirish.io.IODSL._
import net.jackadull.dirish.io._
import net.jackadull.dirish.marshalling.{ProjectConfigParser, TokenToProjectConfig}
import net.jackadull.dirish.model.ProjectConfig

import scala.language.higherKinds

object LoadInternalDB extends IOOp[LoadInternalDBResult] {
  def instantiate[I[+_]](io:IO[I]):I[LoadInternalDBResult] = internal instantiate io

  private val internal:IOOp[LoadInternalDBResult] = ParameterValue(InternalDBFilePath) flatMap {path ⇒
    GetFileInfo(path) flatMap {
      case FileInfoResult(_:PlainFileInfo) ⇒ ReadFileAsString(path, UTF_8) map {
        case err:IOError ⇒ ErrorLoadingInternalDB(err)
        case StringIOResult(fileContents) ⇒ ProjectConfigParser.parse(ProjectConfigParser.root, fileContents) match {
          case ProjectConfigParser.Success(configToken, _) ⇒ TokenToProjectConfig(configToken) match {
            case Right(config) ⇒ InternalDBLoaded(config)
            case Left(err) ⇒ ErrorLoadingInternalDB(CustomIOError(s"Semantical error in internal DB: $err"))
          }
          case err:ProjectConfigParser.NoSuccess ⇒ ErrorLoadingInternalDB(CustomIOError(s"Internal DB parsing error: $err"))
        }
      }
      case FileInfoResult(_:NonExistingFileInfo) ⇒ IOBind(InternalDBEmpty)
      case FileInfoResult(_) ⇒ IOBind(ErrorLoadingInternalDB(CustomIOError(s"Expected the internal DB file at '$path', but found something that is not a file.")))
      case err:IOError ⇒ IOBind(ErrorLoadingInternalDB(err))
    }
  }
}

sealed trait LoadInternalDBResult
sealed trait InternalDBAvailable extends LoadInternalDBResult {def internalDB:ProjectConfig}
final case class InternalDBLoaded(internalDB:ProjectConfig) extends InternalDBAvailable
object InternalDBEmpty extends InternalDBAvailable {def internalDB:ProjectConfig = ProjectConfig.empty}
sealed trait InternalDBUnavailable extends LoadInternalDBResult {def error:IOError}
final case class ErrorLoadingInternalDB(error:IOError) extends InternalDBUnavailable
