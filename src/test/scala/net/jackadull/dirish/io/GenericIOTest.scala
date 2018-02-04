package net.jackadull.dirish.io

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

import net.jackadull.dirish.io.IODSL._
import net.jackadull.dirish.path.UserHomePathSpec
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

import scala.language.{higherKinds, postfixOps}

trait GenericIOTest[I[+_],A<:IO[I]] extends Matchers with BeforeAndAfterAll {
  this:FreeSpec ⇒

  type IOContext<:AnyRef

  protected def createIO():IOContext
  protected def getIO(context:IOContext):A
  protected def ioImplementationName:String
  protected def tearDownIO(context:IOContext)

  private val home = UserHomePathSpec

  ioImplementationName - {
    "creating and then moving a simple directory to trash works" in {
      val testDirectory = home/"simple-test-directory"

      val ioop:IOOp[IOResult] = IOSeq(Seq(
        CreateDirectory(testDirectory),
        GetFileInfo(testDirectory) expecting {case FileInfoResult(DirectoryFileInfo(`testDirectory`)) ⇒ IOSuccess},
        MoveToTrash(testDirectory),
        GetFileInfo(testDirectory) expecting {case FileInfoResult(NonExistingFileInfo(`testDirectory`)) ⇒ IOSuccess}
      ))

      executionResult(ioop instantiate io) match {
        case error:IOError ⇒ fail(error toString)
        case _ ⇒ succeed
      }
    }
  }

  def io:A = ioContainer get() getOrElse(sys error "Called outside of a test (no IO instance)")
  private val ioContainer:AtomicReference[Option[A]] = new AtomicReference(None)
  private val ioContextContainer:AtomicReference[Option[IOContext]] = new AtomicReference(None)

  override protected def afterAll() = {
    ioContextContainer get() match {
      case Some(ctx) ⇒ tearDownIO(ctx); ioContextContainer set None; ioContainer set None
      case None ⇒ ioContextContainer set None; ioContainer set None
    }
    super.afterAll()
  }

  override protected def beforeAll() = {
    super.beforeAll()
    ioContextContainer set None; ioContainer set None
    val ctx = createIO()
    ioContextContainer set Some(ctx); ioContainer set Some(getIO(ctx))
  }

  private def executionResult[B](i:I[B]):B = {
    val fut = new java.util.concurrent.CompletableFuture[B]()
    io.markForExecution(io.map(i)(v ⇒ fut.complete(v)))
    fut.get(5, TimeUnit.SECONDS)
  }
}
