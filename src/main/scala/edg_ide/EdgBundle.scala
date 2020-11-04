package edg_ide

import com.intellij.AbstractBundle
import org.jetbrains.annotations.PropertyKey

object EdgBundleResources {
  final val BUNDLE = "messages.EdgBundle"
}

object EdgBundle extends AbstractBundle(EdgBundleResources.BUNDLE) {
  final val BUNDLE = EdgBundleResources.BUNDLE  // TODO cleanup
  final val INSTANCE = this

  def message(@PropertyKey(resourceBundle = BUNDLE) key: String, params: Any*): String = {
    getMessage(key, params)
  }
}
