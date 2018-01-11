package net.jackadull.dirish.model

import java.util.UUID

/** Defines a base directory, by giving it a globally unique, unchangeable ID, and an absolute path, which may change
  * over time. */
final case class BaseDirectory(id:UUID, path:AbsolutePathSpec)
