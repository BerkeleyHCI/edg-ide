package edg_ide.util

object CrossProductUtils {
  /** Returns the cross product of elements of lists, eg
    * crossProduct([[0, 1], [10, 11]]) => [[0, 10], [0, 11], [1, 10], [1, 11]]
    *
    * Adapted from answers in
    * https://stackoverflow.com/questions/8321906/lazy-cartesian-product-of-several-seqs-in-scala
    */
  def crossProduct[T](lists: Iterable[Iterable[T]]): Iterable[Seq[T]] = {
    lists match {
      case Nil => Iterable(Seq())  // empty base case
      case lists =>
        crossProduct(lists.init).flatMap { initCrossProductElement =>  // for cross of all but first list
          lists.last.map { lastElt =>  // then iterate through options of last list
            initCrossProductElement.appended(lastElt)  // and append each option
          }
        }
    }
  }
}
