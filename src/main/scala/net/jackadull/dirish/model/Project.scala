package net.jackadull.dirish.model

import java.util.UUID

/** Refers to a project as a unique concept. The `id` is globally unique, whereas the `displayName` is only
  * descriptive and may change over time. */
final case class Project(id:UUID, displayName:String) {
  def at(baseDirectory:BaseDirectory):ProjectDirectory = ProjectDirectory(id, baseDirectory id, RelativePathSpec.Empty)
  def gitModule(remotes:Seq[(String,String)]):GitModule = GitModule(id, remotes)

  override def toString = s"$displayName ($id)"
}
