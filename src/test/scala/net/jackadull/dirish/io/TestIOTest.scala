package net.jackadull.dirish.io

import org.scalatest.FreeSpec

class TestIOTest extends FreeSpec with GenericIOTest[TestIOV, TestIO] {
  type IOContext = TestIO

  protected def createIO() = new TestIO
  protected def getIO(context:TestIO) = context
  protected def ioImplementationName = "TestIO"
  protected def tearDownIO(context:TestIO) {}
}
