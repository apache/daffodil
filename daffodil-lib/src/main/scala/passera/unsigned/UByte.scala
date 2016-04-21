package passera.unsigned


case class UByte(override val byteValue: Byte) extends AnyVal with SmallUInt[UByte] with Serializable {
  override def intValue = byteValue & 0xff
}

object UByte {
  def MinValue = UByte(0)
  def MaxValue = UByte(~0)
}
