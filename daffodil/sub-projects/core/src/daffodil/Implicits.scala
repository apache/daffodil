package daffodil

import java.io.{ByteArrayInputStream, BufferedInputStream}
//import daffodil.schema.annotation.AttributeValue
//import daffodil.xml.XMLUtil

object Implicits {
 /**
   * makes tests easier to cope with. can write processor.setTerminator(";") instead of having
   * to convert.
   */
 // implicit def string2AttributeValue(value:String): AttributeValue = XMLUtil getListFromValue(value)
  
  /**
   * Convenience: automatically create buffered stream when needed.
   */
  implicit def byteArrayInputStream2bufferedInputStream(bais : ByteArrayInputStream) : BufferedInputStream =
    new BufferedInputStream(bais)
  
  /**
 * Used for reading/writing to database, files, etc.
 * Code From the book "Beginning Scala"
 * http://www.amazon.com/Beginning-Scala-David-Pollak/dp/1430219890
 */
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
      try { f(param) } finally { param.close() }

}