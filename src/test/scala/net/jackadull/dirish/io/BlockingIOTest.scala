package net.jackadull.dirish.io

import java.nio.file.{Files, Path}

import org.scalatest.FreeSpec

import scala.language.postfixOps

class BlockingIOTest extends FreeSpec with GenericIOTest[BlockingV,BlockingIO] {
  type IOContext = (BlockingIO,Path)

  protected def createIO() =
    {val tmp = Files.createTempDirectory("blocking-io-test"); (new BlockingIO(tmp.toString), tmp)}
  protected def getIO(context:(BlockingIO,Path)) = context._1
  protected def ioImplementationName = "BlockingIO"
  protected def tearDownIO(context:(BlockingIO,Path)) = Files.delete(context _2) // TODO recursively delete
}
