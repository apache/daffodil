package passera.unsigned

import scala.math.{ ScalaNumber, ScalaNumericConversions }

case class UShort(override val shortValue: Short) extends AnyVal with SmallUInt[UShort] with Serializable {
  override def intValue = shortValue & 0xffff
}

object UShort {
  def MinValue = UShort(0)
  def MaxValue = UShort(~0)
}
