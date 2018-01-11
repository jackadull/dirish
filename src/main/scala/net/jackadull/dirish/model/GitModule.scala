package net.jackadull.dirish.model

import java.util.UUID

/** Configuration for a certain project as a Git module. */
final case class GitModule(projectID:UUID, remotes:Seq[(String,String)])
