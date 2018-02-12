package net.jackadull.dirish.main

import net.jackadull.dirish.io.{BlockingIO, IOError}
import net.jackadull.dirish.workflow.locking.LockGuarded
import net.jackadull.dirish.workflow.main.UpdateDirectoryStructure

object Main extends App {
  val io = BlockingIO
  io.markForExecution(io.map(LockGuarded(UpdateDirectoryStructure).instantiate(io)) {
    case err:IOError ⇒ println(s"Error: $err")
    case _ ⇒ ()
  })
}
