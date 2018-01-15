package net.jackadull.dirish.model.project

import java.util.UUID

import net.jackadull.dirish.model.fs.{BaseDirectory, ProjectDirectory, RelativePathSpec}

/** Refers to a project as a unique concept. The `id` is globally unique, whereas the `displayName` is only
  * descriptive and may change over time. */
final case class Project(id:UUID, displayName:String) {
  def at(baseDirectory:BaseDirectory):ProjectDirectory = ProjectDirectory(id, baseDirectory id, RelativePathSpec.Empty)
  def gitModule(remotes:Seq[(String,String)]):GitModule = GitModule(id, remotes)

  override def toString = s"$displayName ($id)"
}
