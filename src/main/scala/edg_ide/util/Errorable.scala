package edg_ide.util


sealed trait Errorable[+T] {
  // Default map function on the contained type, that preserves the first error message,
  // and turns null results into an error.
  def map[V](errMsg: => String)(fn: T => V): Errorable[V] = {
    map[V](errMsg, null.asInstanceOf[V])(fn)
  }

  def map[V](errMsg: => String, failureVal: V)(fn: T => V): Errorable[V]

  // This map doesn't check for result failure, but will propagate parent errors
  def map[V](fn: T => V): Errorable[V]

  // Convenience wrapper for options
  def flatMap[V](errMsg: => String)(fn: T => Option[V]): Errorable[V] = {
    map[V](errMsg, null.asInstanceOf[V]) {
      fn(_) match {
        case Some(result) => result
        case None => null.asInstanceOf[V]
      }
    }
  }

  def +[T2](other: Errorable[T2]): Errorable[(T, T2)] = {
    (this, other) match {
      case (Errorable.Success(thisVal), Errorable.Success(otherVal)) =>
        Errorable.Success((thisVal, otherVal))
      case (thisErr @ Errorable.Error(_), _) => thisErr
      case (_, otherErr @ Errorable.Error(_)) => otherErr
    }
  }
}


object Errorable {
  case class Success[T](obj: T) extends Errorable[T] {
    override def map[V](errMsg: => String, failureVal: V)(fn: T => V): Errorable[V] = {
      val result = fn(obj)
      if (result == failureVal) {
        Error(errMsg)
      } else {
        Success(result)
      }
    }
    override def map[V](fn: T => V): Errorable[V] = {
      Success(fn(obj))
    }
  }
  case class Error(msg: String) extends Errorable[Nothing] {
    override def map[V](errMsg: => String, failureVal: V)(fn: Nothing => V): Errorable[V] = {
      this
    }
    override def map[V](fn: Nothing => V): Errorable[V] = {
      this
    }
  }

  def apply[T](obj: Option[T], errMsg: => String): Errorable[T] = {
    if (obj == null) {  // in case a null goes down this path
      Error(errMsg)
    } else {
      obj match {
        case Some(obj) => Success(obj)
        case None => Error(errMsg)
      }
    }
  }

  def apply[T](obj: T, errMsg: => String): Errorable[T] = {
    apply[T](obj, errMsg, null.asInstanceOf[T])
  }

  def apply[T](obj: T, errMsg: => String, failureVal: T): Errorable[T] = {
    if (obj == failureVal) {
      Error(errMsg)
    } else {
      Success(obj)
    }
  }
}