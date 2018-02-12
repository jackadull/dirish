package net.jackadull.dirish.workflow.main

import net.jackadull.dirish.io.IODSL.{IOBind, IOOp}
import net.jackadull.dirish.io.{CustomIOError, IO, IOResult, IOSuccess}
import net.jackadull.dirish.migration.Migration
import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.workflow.storage._

import scala.language.{higherKinds, postfixOps}

object UpdateDirectoryStructure extends IOOp[IOResult] {
  def instantiate[I[+_]](io:IO[I]):I[IOResult] = internal instantiate io

  private val internal:IOOp[IOResult] = LoadUserConfig flatMap {
    case UserConfigLoaded(userConfig) ⇒ LoadInternalDB flatMap {
      case available:InternalDBAvailable ⇒
        val internalDB = available.internalDB
        Migration(ProjectConfig.changesBetween(internalDB, userConfig), internalDB) map {endResult ⇒
          endResult.errorOpt match {
            case Some(err) ⇒ err
            case None ⇒
              if(endResult.failures isEmpty) IOSuccess
              else CustomIOError("Update failures:\n" +
                endResult.failures.map({case (change, error) ⇒ s"- Could not perform '$change' because: $error"}).toSeq.sorted.mkString("\n")
              )
          }
        }
      case unavailable:InternalDBUnavailable ⇒ IOBind(unavailable error)
    }
    case CannotLoadUserConfig(err) ⇒ IOBind(err)
  }
}
