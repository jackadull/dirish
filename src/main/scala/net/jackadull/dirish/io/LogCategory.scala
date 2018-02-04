package net.jackadull.dirish.io

sealed trait LogCategory
object LogCategory {
  object BeforeChange extends LogCategory
  object FailedChange extends LogCategory
  object PerformedChange extends LogCategory
  object SkippedChangeForDownstreamChange extends LogCategory
}
