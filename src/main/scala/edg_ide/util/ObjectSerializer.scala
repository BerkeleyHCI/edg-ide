package edg_ide.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.util.Base64
import scala.reflect.{ClassTag, classTag}

// shared object serialization utils for saving object state to IDE state, eg persisting
// run configs between IDE restarts
//
// TODO: this is pretty brittle if the internal class changes since the format is tied to the class
// perhaps investigate other serialization strategies, eg uPickle
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

  // after deserializing, test if an object is a Seq of all EltType
  // this version provides an optional elementFn that is run after the element isInstance check
  // as an explicit check on the elements to deal with type erasure
  def optionInstanceOfSeq[EltType <: Object: ClassTag](
      obj: Object,
      elementFn: EltType => Boolean
  ): Option[Seq[EltType]] = obj match {
    case obj: Seq[Any]
      if obj.forall(elt =>
        classTag[EltType].runtimeClass.isInstance(elt) &&
          elementFn(elt.asInstanceOf[EltType])
      ) =>
      Some(obj.asInstanceOf[Seq[EltType]])
    case _ => None
  }

  // this basic version does a simple isInstance check but may provide false negatives on parameterized types
  // including tuples
  def optionInstanceOfSeq[EltType <: Object: ClassTag](obj: Object): Option[Seq[EltType]] = {
    optionInstanceOfSeq[EltType](obj, { x: EltType => true })
  }
}
