package daffodil

import java.io.{ByteArrayInputStream, BufferedInputStream}
import daffodil.schema.annotation.AttributeValue
import daffodil.xml.XMLUtil

object Implicits {
 /**
   * makes tests easier to cope with. can write processor.setTerminator(";") instead of having
   * to convert.
   */
  implicit def string2AttributeValue(value:String): AttributeValue = XMLUtil getListFromValue(value)
  
  /**
   * Convenience: automatically create buffered stream when needed.
   */
  implicit def byteArrayInputStream2bufferedInputStream(bais : ByteArrayInputStream) : BufferedInputStream =
    new BufferedInputStream(bais)
}