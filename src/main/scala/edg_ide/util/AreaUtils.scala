package edg_ide.util

import scala.math.Numeric.Implicits.infixNumericOps

object AreaUtils {
  // Given a set of unordered lines as ((x0, y0), (x1, y1)), returns the ordered set of points
  // that is the closed path formed by those lines, if exactly one exists.
  def closedPathOf[PointType](lines: Seq[(PointType, PointType)]): Option[Seq[PointType]] = {
    val adjacency = lines
      .flatMap { case (p0, p1) =>
        Seq(p0 -> p1, p1 -> p0)
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

    if (lines.isEmpty) { // otherwise lines.head fails
      return None
    }

    // Finds a closed path starting at the end of prevPoints and ending at startPoint, or returning None
    // if no closed path exists
    // seenPoints can exclude startPoint since it's handled separately
    // this uses the above adjacency structure
    def makeClosedPath(
        startPoint: PointType,
        prevPoints: Seq[PointType],
        seenPoints: Set[PointType]
    ): Option[Seq[PointType]] = {
      adjacency.get(prevPoints.last) match {
        case None => None
        case Some(connectedPoints) =>
          // discard back-edge
          val newPoints = connectedPoints.filter(_ != prevPoints(prevPoints.size - 2))
          newPoints match {
            case Seq(nextPoint) if nextPoint == startPoint => Some(prevPoints)
            case Seq(nextPoint) if seenPoints.contains(nextPoint) => None // looped back mid-path
            case Seq(nextPoint) =>
              makeClosedPath(startPoint, prevPoints :+ nextPoint, seenPoints + nextPoint)
            case _ =>
              None // if there isn't exactly one new point
          }
      }
    }
    val possiblePath = makeClosedPath(lines.head._1, Seq(lines.head._1, lines.head._2), Set(lines.head._2))
    possiblePath.flatMap { possiblePath =>
      if (possiblePath.size != lines.size) {
        None // not all lines were used
      } else {
        Some(possiblePath)
      }
    }
  }

  // Calculates twice the area area enclosed by some path.
  // from https://www.geeksforgeeks.org/slicker-algorithm-to-find-the-area-of-a-polygon-in-java/
  def doubleAreaOf[NumType: Numeric](
      lines: Seq[((NumType, NumType), (NumType, NumType))]
  ): Option[NumType] = {
    val orderedPath = closedPathOf(lines)
    orderedPath.map { orderedPath =>
      (orderedPath :+ orderedPath.head)
        .sliding(2)
        .map { case Seq((x0, y0), (x1, y1)) =>
          (x1 * y0) - (x0 * y1)
        }
        .sum
        .abs
    }
  }
}
