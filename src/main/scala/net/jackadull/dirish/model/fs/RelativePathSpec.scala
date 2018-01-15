package net.jackadull.dirish.model.fs

// TODO
sealed trait RelativePathSpec {
  def /(pathElement:String):RelativePathSpec
}
object RelativePathSpec {
  object Empty extends RelativePathSpec {
    def /(pathElement:String):RelativePathSpec = ??? // TODO
  }
}
