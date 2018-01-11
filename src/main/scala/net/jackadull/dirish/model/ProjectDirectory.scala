package net.jackadull.dirish.model

import java.util.UUID

/** Connects a project to a directory, by specifying the ID of a base directory and the relative path below it. */
final case class ProjectDirectory(projectID:UUID, baseDirectoryID:UUID, relativePath:RelativePathSpec) {
  def /(pathElement:String):ProjectDirectory = copy(relativePath = relativePath / pathElement)
}
