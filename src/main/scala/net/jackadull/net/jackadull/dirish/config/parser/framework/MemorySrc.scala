package net.jackadull.net.jackadull.dirish.config.parser.framework

object MemorySrc {
  object Read extends Src[ReadState] {
    override def apply(s:ReadState[Any], char:Char):ReadState[Unit] = s match {
      case f:ReadState.Failure => f
      case ReadState.Success(_, index, sourceMap) if index>=sourceMap.text.length => ReadState.Failure("Unexpected EOF")
      case ReadState.Success(_, index, sourceMap) =>
        if(sourceMap.text(index) == char) ReadState.Success((), index+1, sourceMap)
        else ReadState.Failure(s"Expected '$char'") // TODO proper character escaping
    }

    override def copy[A](from:ReadState[A], to:ReadState[Any]):ReadState[A] = (from, to) match {
      case (_, f:ReadState.Failure) => f
      case (f:ReadState.Failure, _) => f // TODO copy meta info from `to`
      case (a:ReadState.Success[A], b:ReadState.Success[Any]) => b.copy(value = a.value)
    }

    override def fail[A](s:ReadState[A], message:String):ReadState[A] = ReadState.Failure(message)

    override def flatMap[A,A2](s:ReadState[A])(f:A=>ReadState[A2]):ReadState[A2] = s match {
      case f:ReadState.Failure => f
      case a:ReadState.Success[A] => f(a.value)
    }

    override def isSuccess(s:ReadState[Any]):Boolean = s.isInstanceOf[ReadState.Success[Any]]

    override def map[A,A2](s:ReadState[A])(f:A=>A2):ReadState[A2] = s match {
      case f:ReadState.Failure => f
      case a:ReadState.Success[A] => a.copy(value = f(a.value))
    }

    override def set[A](s:ReadState[Any], v:A):ReadState[A] = s match {
      case f:ReadState.Failure => f
      case a:ReadState.Success[Any] => a.copy(value = v)
    }
  }

  sealed trait ReadState[+A]
  object ReadState {
    final case class Failure(message:String) extends ReadState[Nothing]
    final case class Success[+A](value:A, index:Int, sourceMap:SourceMap) extends ReadState[A]

    final case class SourceMap(text:String) {
      override def toString:String = s"SourceMap(...[${text.length}])"
    }
  }
}
