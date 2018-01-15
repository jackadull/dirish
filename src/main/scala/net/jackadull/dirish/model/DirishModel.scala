package net.jackadull.dirish.model

import net.jackadull.dirish.model.fs.{BaseDirectory, ProjectDirectory}
import net.jackadull.dirish.model.project.{GitModule, Project}

/** Serves as the model root, giving access to all other model entities. */
trait DirishModel {
  def baseDirectories:Set[BaseDirectory]
  def projects:Set[Project]
  def projectDirectories:Set[ProjectDirectory]
  def gitModules:Set[GitModule]
}
