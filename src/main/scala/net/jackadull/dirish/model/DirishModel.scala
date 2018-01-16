package net.jackadull.dirish.model

import net.jackadull.dirish.model.fs.{BaseDirectory, ObsoleteProjectsDirectory, ProjectDirectory}
import net.jackadull.dirish.model.project.{GitModule, Project}

/** Serves as the model root, giving access to all other model entities. */
trait DirishModel {
  def baseDirectories:Set[BaseDirectory]
  def gitModules:Set[GitModule]
  def obsoleteProjectsDirectory:ObsoleteProjectsDirectory
  def projects:Set[Project]
  def projectDirectories:Set[ProjectDirectory]

  def modGitModules(f:Set[GitModule]⇒Set[GitModule]):DirishModel
  def modProjects(f:Set[Project]⇒Set[Project]):DirishModel
  def modProjectDirectories(f:Set[ProjectDirectory]⇒Set[ProjectDirectory]):DirishModel
}
