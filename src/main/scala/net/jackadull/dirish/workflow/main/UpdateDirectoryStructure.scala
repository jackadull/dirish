package net.jackadull.dirish.workflow.main

import net.jackadull.dirish.migration.Migration
import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.combinator.{FailWith, ResultIn}
import net.jackadull.dirish.op.{GenericMessageError, OpError}
import net.jackadull.dirish.workflow.storage.{LoadInternalDB, LoadUserConfig}

import scala.language.postfixOps

object UpdateDirectoryStructure extends ProxyOp[Unit,OpError,MigrationStyle] {
  protected val innerOp = LoadUserConfig >> {userConfig ⇒ LoadInternalDB >>[Unit,OpError,MigrationStyle] {internalDB ⇒
    Migration(ProjectConfig changesBetween (internalDB, userConfig), internalDB) >> {migrationResult ⇒
      migrationResult errorOpt match {
        case Some(error) ⇒ FailWith(error)
        case None ⇒
          if(migrationResult.failures isEmpty) ResultIn success
          else FailWith(GenericMessageError("Update failures:\n" +
            migrationResult.failures.map({case (change, error) ⇒ s"- Could not perform '$change' because: $error"}).toSeq.sorted.mkString("\n"))
          )
      }
    }
  }}
}
