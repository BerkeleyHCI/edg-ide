package edg_ide.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.util.Base64


// shared object serialization utils for saving object state to IDE state, eg persisting
// run configs between IDE restarts
object ObjectSerializer {
  def serialize(obj: Object): String = {
    val out = new ByteArrayOutputStream()
    val os = new ObjectOutputStream(out)
    os.writeObject(obj)
    Base64.getEncoder.encodeToString(out.toByteArray)
  }

  def deserialize(str: String): Option[AnyRef] = {
    try {
      val bytes = Base64.getDecoder.decode(str)
      val is = new ObjectInputStream(new ByteArrayInputStream(bytes))
      Option(is.readObject())
    } catch {
      case _: IOException | _: ClassNotFoundException => None
    }
  }
}
