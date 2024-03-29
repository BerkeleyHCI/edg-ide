package edg_ide.util

object SeqUtils {
  // if all elements defined, returns the seq of elements, else None
  def getAllDefined[T](seq: Seq[Option[T]]): Option[Seq[T]] = {
    val (some, none) = seq.partitionMap {
      case Some(value) => Left(value)
      case None => Right(None)
    }
    if (none.nonEmpty) {
      None
    } else {
      Some(some)
    }
  }
}
