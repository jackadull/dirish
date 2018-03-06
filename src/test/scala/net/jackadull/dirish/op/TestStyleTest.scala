package net.jackadull.dirish.op

import net.jackadull.dirish.op.combinator.EitherV
import net.jackadull.dirish.op.io.GenericIOTest
import org.scalatest.FreeSpec

class TestStyleTest extends FreeSpec with GenericIOTest {
  type TestIOV[+R,+E] = EitherV[R,E]
  type TestIOStyle = TestStyle
  type TestIOContext = TestIOStyle

  protected def createIO() = new TestIOStyle
  protected def getIOStyle(context:TestIOStyle) = context
  protected def ioImplementationName = "TestIO"
  protected def tearDownIO(context:TestIOStyle) {}
}
