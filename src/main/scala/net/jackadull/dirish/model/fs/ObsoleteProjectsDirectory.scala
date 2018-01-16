package net.jackadull.dirish.model.fs

import java.util.UUID

/** Determines where project directories will be moved when they got removed. They are not deleted straight away
  * because they might contain changes that have not been stored. The user should regularly review the obsolete
  * projects directory, and remove entries that are really not needed any more. */
final case class ObsoleteProjectsDirectory(baseDirectoryID:UUID, relativePath:RelativePathSpec)
