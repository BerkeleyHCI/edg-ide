package edg_ide.util

object IterableExtensions {
  implicit class IterableExtension[T](iterable: Iterable[T]) {
    // If the iterable contains all of the same value (even if it's the only value), returns that value.
    // Otherwise, or if the iterable is empty, return None
    def allSameValue: Option[T] = {
      val headValue = iterable.headOption
      headValue match {
        case Some(headValue) =>
          if (iterable.forall(_ == headValue)) {
            Some(headValue)
          } else {
            None
          }
        case None =>
          None
      }
    }
  }
}
