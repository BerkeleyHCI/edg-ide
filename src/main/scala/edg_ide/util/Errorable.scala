package edg_ide.util


sealed trait Errorable[+T] {
  // Default map function on the contained type, that preserves the first error message,
  // and turns null results into an error.
  def mapWithErr[V](errMsg: => String)(fn: T => V): Errorable[V] = {
    mapWithErr[V](errMsg, null.asInstanceOf[V])(fn)
  }

  def mapWithErr[V](errMsg: => String, failureVal: V)(fn: T => V): Errorable[V]
}


object Errorable {
  case class Success[T](obj: T) extends Errorable[T] {
    override def mapWithErr[V](errMsg: => String, failureVal: V)(fn: T => V): Errorable[V] = {
      val result = fn(obj)
      if (result == failureVal) {
        Error(errMsg)
      } else {
        Success(result)
      }
    }
  }
  case class Error(msg: String) extends Errorable[Nothing] {
    override def mapWithErr[V](errMsg: => String, failureVal: V)(fn: Nothing => V): Errorable[V] = {
      this
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
