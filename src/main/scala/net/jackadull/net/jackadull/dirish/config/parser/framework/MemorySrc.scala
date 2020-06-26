package net.jackadull.net.jackadull.dirish.config.parser.framework

import scala.annotation.tailrec

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

  object Write extends Src[WriteState] {
    override def apply(s:WriteState[Any], char:Char):WriteState[Unit] = s match {
      case f:WriteState.Failure => f
      case s:WriteState.Success[Any] => WriteState.WriteChar((), char, Some(s))
    }

    override def copy[A](from:WriteState[A], to:WriteState[Any]):WriteState[A] = to match {
      case f:WriteState.Failure => f
      case to2:WriteState.Success[A] => from match {
        case f:WriteState.Failure => f // TODO copy meta info from `to`
        case from2:WriteState.Success[A] => to2.set(from2.value)
      }
    }

    override def fail[A](s:WriteState[A], message:String):WriteState[A] = WriteState.Failure(message)

    override def flatMap[A,A2](s:WriteState[A])(f:A=>WriteState[A2]):WriteState[A2] = s match {
      case f:WriteState.Failure => f
      case w:WriteState.Success[A] => f(w.value)
    }

    override def isSuccess(s:WriteState[Any]):Boolean = s.isInstanceOf[WriteState.Success[Any]]

    override def map[A,A2](s:WriteState[A])(f:A=>A2):WriteState[A2] = s match {
      case f:WriteState.Failure => f
      case w:WriteState.Success[A] => w.set(f(w.value))
    }

    override def set[A](s:WriteState[Any], v:A):WriteState[A] = s match {
      case f:WriteState.Failure => f
      case w:WriteState.Success[A] => w.set(v)
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

  sealed trait WriteState[+A]
  object WriteState {
    sealed trait Success[+A] extends WriteState[A] {
      def length:Int = {
        @tailrec def recurse(w:Success[Any], l:Int):Int = w.predecessor match {
          case None => l + w.individualLength
          case Some(s) => recurse(s, l+w.individualLength)
        }
        recurse(this, 0)
      }

      override def toString:String = {
        val builder = new StringBuilder(length)
        @tailrec def recurse(w:Success[Any]):String = {
          w match {
            case s:WriteChar[Any] => builder.append(s.char)
          }
          w.predecessor match {
            case None => builder.toString()
            case Some(pred) => recurse(pred)
          }
        }
        recurse(this)
      }

      def individualLength:Int
      def predecessor:Option[Success[Any]]
      def set[A2](v:A2):Success[A2]
      def value:A
    }

    final case class Failure(message:String) extends WriteState[Nothing]
    final case class WriteChar[+A](value:A, char:Char, predecessor:Option[Success[Any]]) extends Success[A] {
      override def individualLength:Int = 1
      override def set[A2](v:A2):Success[A2] = copy(value = v)
    }
  }
}