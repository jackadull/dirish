package net.jackadull.dirish.main

import net.jackadull.dirish.op.BlockingEitherStyles
import net.jackadull.dirish.op.combinator.FailWith
import net.jackadull.dirish.workflow.locking.LockGuarded
import net.jackadull.dirish.workflow.main.UpdateDirectoryStructure

object Main extends App {
  val mainOp = LockGuarded(UpdateDirectoryStructure) #>> {
    err ⇒ println(s"Error: $err"); FailWith(err)
  }
  mainOp instantiateIn BlockingEitherStyles
}
