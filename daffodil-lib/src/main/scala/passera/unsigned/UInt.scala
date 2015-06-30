package passera.unsigned

import scala.math.{ ScalaNumber, ScalaNumericConversions }

class UInt(val intValue: Int) extends AnyVal with SmallUInt[UInt] with Serializable {
  override def toUInt = this
  private def intRep = intValue
}

object UInt {
  def MinValue = UInt(0)
  def MaxValue = UInt(~0)

  def apply(x: Int) = new UInt(x)
  def unapply(x: UInt) = Some((x.intValue))
}

