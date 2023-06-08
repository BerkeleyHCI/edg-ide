package edg_ide.swing

import java.awt.Color

object ColorUtil {
  def blendColor(baseColor: Color, topColor: Color, factor: Double): Color = {
    new Color(
      (baseColor.getRed * (1 - factor) + topColor.getRed * factor).toInt,
      (baseColor.getGreen * (1 - factor) + topColor.getGreen * factor).toInt,
      (baseColor.getBlue * (1 - factor) + topColor.getBlue * factor).toInt,
      baseColor.getAlpha
    )
  }

  def withAlpha(baseColor: Color, alpha: Int): Color = {
    new Color(
      baseColor.getRed,
      baseColor.getGreen,
      baseColor.getBlue,
      alpha
    )
  }
}
