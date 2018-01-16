package net.jackadull.dirish.model.environment

import scala.language.higherKinds

/** Actually executes commands on the filesystem, or requests state. By exchanging the implementation of this
  * trait, the whole I/O behaviour of the application can be changed. */
trait Environment[A[_]] {
  def logInfo(message:String)(implicit io:DirishIO[A]):A[Unit]
  def logWarn(message:String)(implicit io:DirishIO[A]):A[Unit]
}
